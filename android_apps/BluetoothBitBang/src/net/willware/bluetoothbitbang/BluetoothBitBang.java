package net.willware.bluetoothbitbang;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.IOException;
import java.lang.Integer;
import java.lang.NumberFormatException;
import java.lang.Thread;
import java.util.Set;
import java.util.UUID;
import java.util.Timer;
import java.util.TimerTask;

import android.app.Activity;
import android.app.Dialog;
import android.app.AlertDialog;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.widget.Button;
import android.widget.TextView;
import android.widget.LinearLayout;
import android.widget.Toast;
import android.util.Log;
import android.view.Gravity;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.WindowManager.LayoutParams;
import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothSocket;
import android.bluetooth.BluetoothDevice;

public class BluetoothBitBang extends Activity {

    private static final boolean D = true;   // enable logging?

    private static final int NUM_OUTPUTS = 6;
    private static final int NUM_INPUTS = 6;

    // Intent request codes
    //private static final int REQUEST_CONNECT_DEVICE = 1;
    private static final int REQUEST_ENABLE_BT = 2;

    private static final int INPUT_POLLING_PERIOD = 100;

    private static final String TAG = "BluetoothBitBang";

    private LinearLayout outputLayout;
    private LinearLayout inputLayout;
    private final OutputBit outputs[] = new OutputBit[NUM_OUTPUTS];
    private final InputBit inputs[] = new InputBit[NUM_INPUTS];
    private BluetoothAdapter adapter;
    private InputPollingThread mInputPollingThread;
    private int inputBits;   // TODO: replace this with something better
    private boolean gotFirstInputResponse = false;
    private boolean wasConnected = false;

    // Message types sent from the BluetoothChatService Handler
    public static final int MESSAGE_STATE_CHANGE = 1;
    public static final int MESSAGE_READ = 2;
    public static final int MESSAGE_WRITE = 3;
    public static final int MESSAGE_DEVICE_NAME = 4;
    public static final int MESSAGE_TOAST = 5;

    public static final String DEVICE_NAME = "device_name";
    public static final String TOAST = "toast";

    private BluetoothComm mComm = null;

    // http://developer.android.com/reference/android/bluetooth/BluetoothDevice.html
    // Hint: If you are connecting to a Bluetooth serial
    // board then try using the well-known SPP UUID...
    private static final UUID MAGIC_SERIAL_UUID =
        UUID.fromString("00001101-0000-1000-8000-00805F9B34FB");

    @Override
    public void onCreate(Bundle savedInstanceState) {
        final int totalPadding = 40;  // paddingLeft + paddingRight
        int i;
        int screenWidth = getResources().getDisplayMetrics().widthPixels;
        int outputWidth = (screenWidth - totalPadding) / NUM_OUTPUTS;
        int inputWidth = (screenWidth - totalPadding) / NUM_INPUTS;
        super.onCreate(savedInstanceState);
        Log.i(TAG, "++ ON CREATE ++");
        setContentView(R.layout.main);
        getWindow().addFlags(LayoutParams.FLAG_KEEP_SCREEN_ON);
        inputLayout = (LinearLayout) findViewById(R.id.inputLayout);
        outputLayout = (LinearLayout) findViewById(R.id.outputLayout);

        // set up input bits
        for (i = 0; i < NUM_INPUTS; i++) {
            InputBit bit = new InputBit(i, inputWidth);
            inputs[i] = bit;
            inputLayout.addView(bit.getUi());
        }

        // set up output bits
        for (i = 0; i < NUM_OUTPUTS; i++) {
            final int index = i;
            OutputBit bit = new OutputBit(i, outputWidth);
            outputs[i] = bit;
            outputLayout.addView(bit.getUi());
        }

        // Get local Bluetooth adapter
        adapter = BluetoothAdapter.getDefaultAdapter();
        // If the adapter is null, then Bluetooth is not supported
        if (adapter == null) {
            Toast.makeText(this, "Bluetooth is not available", Toast.LENGTH_LONG).show();
            finish();
        }
    }

    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (D) Log.d(TAG, "onActivityResult " + resultCode);
        switch (requestCode) {
        case REQUEST_ENABLE_BT:
            // When the request to enable Bluetooth returns
            if (resultCode == Activity.RESULT_OK) {
                // Bluetooth is now enabled, so set up a chat session
                mComm = new BluetoothComm(MAGIC_SERIAL_UUID, mHandler);
            } else {
                // User did not enable Bluetooth or an error occured
                Toast.makeText(this, "Bluetooth is not enabled", Toast.LENGTH_SHORT).show();
                finish();
            }
        }
    }

    @Override
    public void onStart() {
        super.onStart();
        Log.i(TAG, "++ ON START ++");

        // If BT is not on, request that it be enabled.
        // setupChat() will then be called during onActivityResult
        if (!adapter.isEnabled()) {
            Intent enableIntent = new Intent(BluetoothAdapter.ACTION_REQUEST_ENABLE);
            startActivityForResult(enableIntent, REQUEST_ENABLE_BT);
        // Otherwise, setup the chat session
        } else {
            if (mComm == null)
                mComm = new BluetoothComm(MAGIC_SERIAL_UUID, mHandler);
        }
    }

    /**
     * Periodically send the "READ" command to the hardware.
     */
    private class InputPollingThread extends Thread {
        private final static int POLLING_PERIOD = 1000;
        private boolean running = false;
        public void start() {
            running = true;
            Log.i(TAG, "** START INPUT POLLING **");
            super.start();
        }
        public void run() {
            while (running && !gotFirstInputResponse) {
                if (mComm != null) {
                    Log.i(TAG, "** POLL INPUTS **");
                    mComm.write("READ\n".getBytes());
                }
                try {
                    sleep(POLLING_PERIOD);
                }
                catch (InterruptedException e) { }
            }
        }
        public void cancel() {
            Log.i(TAG, "** CANCEL INPUT POLLING **");
            running = false;
        }
    }

    @Override
    public void onResume() {
    	super.onResume();
        Log.i(TAG, "++ ON RESUME ++");
    }

    @Override
    public void onPause() {
    	super.onPause();
        Log.i(TAG, "-- ON PAUSE --");
    }

    private final Handler mHandler = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            SharedPreferences settings = getPreferences(MODE_PRIVATE);
            int outputLevels;
            switch (msg.what) {
                case MESSAGE_STATE_CHANGE:
                    switch (msg.arg1) {
                        case BluetoothComm.STATE_CONNECTED:
                            Log.i(TAG, "BluetoothComm.STATE_CONNECTED");
                            wasConnected = true;
                            outputLevels = settings.getInt("outputLevels", 0);
                            for (int i = 0; i < NUM_OUTPUTS; i++)
                                outputs[i].setLevel((outputLevels & (1 << i)) != 0);
                            mInputPollingThread = new InputPollingThread();
                            mInputPollingThread.start();
                            break;
                        case BluetoothComm.STATE_CONNECTING:
                            Log.i(TAG, "BluetoothComm.STATE_CONNECTING");
                            // anything?
                            break;
                        case BluetoothComm.STATE_NONE:
                            Log.i(TAG, "BluetoothComm.STATE_NONE");
                            // connection lost, cancel input polling
                            if (mInputPollingThread != null) {
                                mInputPollingThread.cancel();
                                mInputPollingThread = null;
                            }
                            if (wasConnected) {
                                outputLevels = 0;
                                for (int i = 0; i < NUM_OUTPUTS; i++)
                                    if (outputs[i].getLevel())
                                        outputLevels |= 1 << i;
                                SharedPreferences.Editor editor = settings.edit();
                                editor.putInt("outputLevels", outputLevels);
                                editor.commit();
                                // TODO: try to reconnect here?
                            }
                            wasConnected = false;
                            break;
                    }
                    break;
                case MESSAGE_WRITE:
                    // In the BluetoothChat app, this is an echo where I see what I said
                    String writeMessage = new String((byte[]) msg.obj);
                    Log.i(TAG, "MESSAGE_WRITE: " + writeMessage);
                    break;
                case MESSAGE_READ:
                    byte[] readBuf = (byte[]) msg.obj;
                    // construct a string from the valid bytes in the buffer
                    String readMessage = new String(readBuf, 0, msg.arg1);
                    Log.i(TAG, "MESSAGE_READ: " + readMessage);
                    if (readMessage.startsWith("0x")) {
                        readMessage = readMessage.trim();
                        try {
                            int inputBits =
                                Integer.parseInt(readMessage.substring(2), 16);
                            for (int i = 0; i < 6; i++)
                                inputs[i].setLevel((inputBits & (1 << i)) != 0);
                            // once we've received the first input message, we can depend on the
                            // hardware to update us if any inputs change
                            gotFirstInputResponse = true;
                        } catch (NumberFormatException e) { }
                    }
                    break;
                case MESSAGE_TOAST:
                    Toast.makeText(getApplicationContext(), msg.getData().getString(TOAST),
                                   Toast.LENGTH_SHORT).show();
                    break;
            }
        }
    };

    private class InputBit {
        private final int index, width;
        private boolean level;
        private final TextView display;
        public InputBit(int index, int width) {
            this.width = width;
            this.index = index;
            this.level = false;
            display = new TextView(BluetoothBitBang.this);
            display.setWidth(width);
            display.setTextColor(0xFF000000);
            display.setTextSize(40);
            display.setText("0");
            display.setGravity(Gravity.CENTER);
        }
        public View getUi() {
            LinearLayout layout = new LinearLayout(BluetoothBitBang.this);
            layout.setOrientation(LinearLayout.VERTICAL);
            TextView bitIndex = new TextView(BluetoothBitBang.this);
            bitIndex.setWidth(width);
            bitIndex.setTextColor(0xFF000000);
            bitIndex.setTextSize(40);
            bitIndex.setText("" + index);
            bitIndex.setGravity(Gravity.CENTER);
            layout.addView(bitIndex);
            layout.addView(display);
            return layout;
        }
        public boolean getLevel() {
            //Log.i(TAG, "InputBit.getLevel(" + index + ") = " + (level ? "1" : "0"));
            return level;
        }
        public void setLevel(boolean level) {
            String bitstring = level ? "1" : "0";
            //Log.i(TAG, "InputBit.setLevel(" + index + ", " + bitstring + ")");
            this.level = level;
            display.setText(bitstring);
        }
    }

    private class OutputBit {
        private final int index, width;
        private boolean level;
        private final TextView display;
        public OutputBit(int index, int width) {
            this.width = width;
            this.index = index;
            this.level = false;
            display = new TextView(BluetoothBitBang.this);
            display.setWidth(width);
            display.setTextColor(0xFF000000);
            display.setTextSize(40);
            display.setText("0");
            display.setGravity(Gravity.CENTER);
        }
        public View getUi() {
            LinearLayout singleOutputLayout = new LinearLayout(BluetoothBitBang.this);
            singleOutputLayout.setOrientation(LinearLayout.VERTICAL);
            Button setButton = new Button(BluetoothBitBang.this);
            setButton.setWidth(width);
            setButton.setTextSize(25);
            setButton.setText("" + index);
            setButton.setOnClickListener(new OnClickListener() {
                    public void onClick(View v) {
                        setLevel(true);
                    }
                });
            singleOutputLayout.addView(setButton);
            Button clearButton = new Button(BluetoothBitBang.this);
            clearButton.setWidth(width);
            clearButton.setOnClickListener(new OnClickListener() {
                    public void onClick(View v) {
                        setLevel(false);
                    }
                });
            singleOutputLayout.addView(clearButton);
            singleOutputLayout.addView(display);
            return singleOutputLayout;
        }
        public boolean getLevel() {
            return level;
        }
        public void setLevel(boolean level) {
            String scmd = (level ? "SET" : "CLEAR") + " " + index + "\n";
            Log.i(TAG, scmd);
            mComm.write(scmd.getBytes());
            this.level = level;
            display.setText(level ? "1" : "0");
        }
    }
}

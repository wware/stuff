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
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.widget.Button;
import android.widget.TextView;
import android.widget.LinearLayout;
import android.util.Log;
import android.view.Gravity;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.WindowManager.LayoutParams;
import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothSocket;
import android.bluetooth.BluetoothDevice;

public class BluetoothBitBang extends Activity {

    private static final int NUM_OUTPUTS = 6;
    private static final int NUM_INPUTS = 6;

    private static final int BT_NOT_ENABLED_ALERT = 0;
    private static final int HW_NOT_PAIRED_ALERT = 1;

    private static final int INPUT_POLLING_PERIOD = 3000;

    private static final String TAG = "BluetoothBitBang";

    // http://developer.android.com/reference/android/bluetooth/BluetoothDevice.html
    // Hint: If you are connecting to a Bluetooth serial
    // board then try using the well-known SPP UUID...
    private static final UUID MAGIC_SERIAL_UUID =
        UUID.fromString("00001101-0000-1000-8000-00805F9B34FB");

    private LinearLayout outputLayout;
    private LinearLayout inputLayout;
    private final OutputBit outputs[] = new OutputBit[NUM_OUTPUTS];
    private final InputBit inputs[] = new InputBit[NUM_INPUTS];
    private BluetoothAdapter adapter;
    private BluetoothSocket bbbSocket;
    private BluetoothBitBang myself;
    private InputSignalScanner scanner;

    private int inputBits;

    protected Dialog onCreateDialog(int id) {
        AlertDialog.Builder builder = new AlertDialog.Builder(this);
        switch(id) {
            case BT_NOT_ENABLED_ALERT:
                builder.setMessage("Bluetooth is not enabled")
                    .setCancelable(true);
                return builder.create();
            case HW_NOT_PAIRED_ALERT:
                builder.setMessage("Not paired with the hardware")
                    .setCancelable(true);
                return builder.create();
            default:
                return null;
        }
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        final int totalPadding = 40;  // paddingLeft + paddingRight
        int i;
        int screenWidth = getResources().getDisplayMetrics().widthPixels;
        int outputWidth = (screenWidth - totalPadding) / NUM_OUTPUTS;
        int inputWidth = (screenWidth - totalPadding) / NUM_INPUTS;
        super.onCreate(savedInstanceState);
        Log.i(TAG, "onCreate method");
        setContentView(R.layout.main);
        getWindow().addFlags(LayoutParams.FLAG_KEEP_SCREEN_ON);
        inputLayout = (LinearLayout) findViewById(R.id.inputLayout);
        outputLayout = (LinearLayout) findViewById(R.id.outputLayout);
        myself = this;

        ((Button) findViewById(R.id.pollInputs))
            .setOnClickListener(new OnClickListener() {
                    public void onClick(View v) {
                        for (int i = 0; i < 6; i++)
                            inputs[i].setLevel((inputBits & (1 << i)) != 0);
                    }
                });

        Log.i(TAG, "set up input bits");
        for (i = 0; i < NUM_INPUTS; i++) {
            InputBit bit = new InputBit(i, inputWidth);
            inputs[i] = bit;
            inputLayout.addView(bit.getUi());
        }

        Log.i(TAG, "set up output bits");
        for (i = 0; i < NUM_OUTPUTS; i++) {
            final int index = i;
            OutputBit bit = new OutputBit(i, outputWidth);
            bit.setListener(new OutputBitListener() {
                    public void onLevelChanged(boolean level) {
                        if (bbbSocket == null) {
                            inputs[index].setLevel(level);
                        }
                    }
                });
            outputs[i] = bit;
            outputLayout.addView(bit.getUi());
        }
    }

    @Override
    public void onPause() {
    	super.onPause();
        if (scanner != null)
            scanner.kill();
        if (bbbSocket != null) {
            try { bbbSocket.close(); }
            catch (IOException e) { }
        }
        // save states of outputs
        int outputLevels = 0;
        for (int i = 0; i < NUM_OUTPUTS; i++)
            if (outputs[i].getLevel())
                outputLevels |= 1 << i;
        SharedPreferences settings = getPreferences(MODE_PRIVATE);
        SharedPreferences.Editor editor = settings.edit();
        editor.putInt("outputLevels", outputLevels);
        editor.commit();
    }

    @Override
    public void onResume() {
    	super.onResume();
        Log.i(TAG, "check if bluetooth is enabled");
        adapter = BluetoothAdapter.getDefaultAdapter();
        if (adapter == null || !adapter.isEnabled()) {
            Log.i(TAG, "Bluetooth not enabled, post alert dialog");
            showDialog(BT_NOT_ENABLED_ALERT);
        } else {
            Log.i(TAG, "Hooray, Bluetooth is enabled");
            //adapter.startDiscovery();
            boolean deviceFound = false;
            Set<BluetoothDevice> pairedDevices = adapter.getBondedDevices();
            if (pairedDevices.size() > 0) {
                Log.i(TAG, "Found at least one paired Bluetooth device");
                for (BluetoothDevice device : pairedDevices) {
                    Log.i(TAG, "Paired with " + device.getName());
                    // Connecting a BluetoothSocket to a BTM-182 is TRICKY!
                    // Google "Celljoust BTCommThread" for clues.
                    if ("Serial Adaptor".equals(device.getName())) {
                        int socketint = 0;
                        while (bbbSocket == null && socketint < 10) {
                            try {
                                adapter.cancelDiscovery();
                                if (socketint == 0) {
                                    bbbSocket = device
                                        .createRfcommSocketToServiceRecord
                                        (MAGIC_SERIAL_UUID);
                                } else {
                                    java.lang.reflect.Method m = device
                                        .getClass()
                                        .getMethod("createRfcommSocket",
                                                   new Class[] { int.class });
                                    bbbSocket = (BluetoothSocket)
                                        m.invoke(device, Integer.valueOf(socketint));
                                }
                                //bbbSocket.setSoTimeout(3000);
                                bbbSocket.connect();
                            }
                            catch (Exception e) {
                                Log.e(TAG, "could not connect", e);
                                bbbSocket = null;
                                socketint++;
                            }
                        }
                        deviceFound = true;
                        break;
                    }
                }
            }
            if (!deviceFound) {
                Log.i(TAG, "BitBang device not found, post alert dialog");
                showDialog(HW_NOT_PAIRED_ALERT);
            }
        }
        SharedPreferences settings = getPreferences(MODE_PRIVATE);
        int outputLevels = settings.getInt("outputLevels", 0);
        for (int i = 0; i < NUM_OUTPUTS; i++)
            outputs[i].setLevel((outputLevels & (1 << i)) != 0);
        if (scanner != null)
            scanner.kill();
        scanner = new InputSignalScanner();
        new Timer().schedule(scanner, INPUT_POLLING_PERIOD, INPUT_POLLING_PERIOD);
    }

    private class InputBit {
        private final int index, width;
        private boolean level;
        private final TextView display;
        public InputBit(int index, int width) {
            this.width = width;
            this.index = index;
            this.level = false;
            display = new TextView(myself);
            display.setWidth(width);
            display.setTextColor(0xFF000000);
            display.setTextSize(40);
            display.setText("0");
            display.setGravity(Gravity.CENTER);
        }
        public View getUi() {
            LinearLayout layout = new LinearLayout(myself);
            layout.setOrientation(LinearLayout.VERTICAL);
            TextView bitIndex = new TextView(myself);
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
            Log.i(TAG, "InputBit.getLevel(" + index + ") = " + (level ? "1" : "0"));
            return level;
        }
        public void setLevel(boolean level) {
            String bitstring = level ? "1" : "0";
            Log.i(TAG, "InputBit.setLevel(" + index + ", " + bitstring + ")");
            this.level = level;
            display.setText(bitstring);
        }
    }

    private void sendString(String s) {
        if (bbbSocket == null)
            return;
        try {
            (new PrintStream(bbbSocket
                             .getOutputStream())).println(s);
        } catch (IOException e) {
            Log.e(TAG, "sendString(\"" + s + "\") FAIL");
            try { bbbSocket.close(); }
            catch (IOException e2) { }
            bbbSocket = null;
        }
    }

    private interface OutputBitListener {
        void onLevelChanged(boolean level);
    }

    private class OutputBit {
        private final int index, width;
        private boolean level;
        private OutputBitListener listener;
        private final TextView display;
        public OutputBit(int index, int width) {
            this.width = width;
            this.index = index;
            this.level = false;
            this.listener = null;
            display = new TextView(myself);
            display.setWidth(width);
            display.setTextColor(0xFF000000);
            display.setTextSize(40);
            display.setText("0");
            display.setGravity(Gravity.CENTER);
        }
        public void setListener(OutputBitListener listener) {
            this.listener = listener;
        }
        public View getUi() {
            LinearLayout singleOutputLayout = new LinearLayout(myself);
            singleOutputLayout.setOrientation(LinearLayout.VERTICAL);
            Button setButton = new Button(myself);
            setButton.setWidth(width);
            setButton.setTextSize(25);
            setButton.setText("" + index);
            setButton.setOnClickListener(new OnClickListener() {
                    public void onClick(View v) {
                        setLevel(true);
                    }
                });
            singleOutputLayout.addView(setButton);
            Button clearButton = new Button(myself);
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
            Log.i(TAG, "OutputBit.getLevel(" + index + ") = " + (level ? "1" : "0"));
            return level;
        }
        public void setLevel(boolean level) {
            String bitstring = level ? "1" : "0";
            Log.i(TAG, "OutputBit.setLevel(" + index + ", " + bitstring + ")");
            this.level = level;
            sendString((level ? "SET" : "CLEAR") + " " + index);
            display.setText(bitstring);
            if (listener != null)
                listener.onLevelChanged(level);
        }
    }

    // doesn't care what thread it runs in
    private void scanInputs() {
        if (bbbSocket != null) {
            Log.i(TAG, "InputSignalScanner: sending READ command");
            sendString("READ");
            Log.i(TAG, "InputSignalScanner: getting response to READ command");
            try {
                BufferedReader br = new
                    BufferedReader(new InputStreamReader(bbbSocket
                                                         .getInputStream()),
                                   80);
                inputBits = Integer.parseInt(br.readLine(), 16);
            } catch (IOException e) {
                Log.i(TAG, "InputSignalScanner: hit an IOException");
            } catch (NumberFormatException e2) {
                Log.i(TAG, "InputSignalScanner: hit a NumberFormatException");
            }
        }
    }

    /*
     * I need to debug this on the SAM7 side. When I do that, this should get
     * simpler and it should stop being necessary to have multiple copies of the
     * scanInputs thread running around.
     *
     * An ideal form of debugger would be a terminal running on the phone that
     * exposed the BluetoothSocket as directly as possible. Maybe I can do that.
     */
    private class InputSignalScanner extends TimerTask {
        private boolean killFlag = false;
        public void kill() {
            killFlag = true;
            cancel();
        }
        public void run() {
            if (!killFlag) {
                new Thread() {
                    public void run() {
                        scanInputs();
                    }
                }.start();
            }
        }
    }
}

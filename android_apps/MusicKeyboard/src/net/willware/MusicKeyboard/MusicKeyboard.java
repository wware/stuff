package net.willware.MusicKeyboard;

import android.app.Activity;
import android.app.AlertDialog;
import android.os.Bundle;
import android.widget.Button;
import android.widget.TextView;
import android.widget.LinearLayout;
import android.view.View;
import android.view.View.OnTouchListener;
import android.view.WindowManager.LayoutParams;
import android.view.MenuInflater;
import android.view.Menu;
import android.view.MenuItem;
import android.view.MotionEvent;
import android.os.Handler;
import android.content.Intent;
import android.content.SharedPreferences;
import android.net.Uri;
import android.util.Log;
import android.media.MediaPlayer;
import android.media.AudioManager;
import android.graphics.LightingColorFilter;

public class MusicKeyboard extends Activity
{
    private ConnectableButton keys[];
    private static final String TAG = "MusicKeyboard";
    private static MusicKeyboard myself;

    // deprecated
    public static final int DEFAULT_DURATION = 30;

    private final int pitches[] = {
        R.raw.tone00, R.raw.tone01, R.raw.tone02, R.raw.tone03, R.raw.tone04, R.raw.tone05,
        R.raw.tone06, R.raw.tone07, R.raw.tone08, R.raw.tone09, R.raw.tone10, R.raw.tone11,
        R.raw.tone12, R.raw.tone13, R.raw.tone14, R.raw.tone15, R.raw.tone16, R.raw.tone17,
        R.raw.tone18, R.raw.tone19, R.raw.tone20, R.raw.tone21, R.raw.tone22, R.raw.tone23,
        R.raw.tone24, R.raw.tone25, R.raw.tone26, R.raw.tone27, R.raw.tone28, R.raw.tone29,
        R.raw.tone30, R.raw.tone31, R.raw.tone32, R.raw.tone33, R.raw.tone34, R.raw.tone35,
        R.raw.tone36
    };

    private final ToneGenerator generators[] = new ToneGenerator[pitches.length];

    private class ConnectableButton extends Button {
        private ToneGenerator tonegen;
        public ConnectableButton(android.content.Context ctx) {
            super(ctx);
        }
        public void setToneGenerator(ToneGenerator tg) {
            tonegen = tg;
        }
        public boolean onTouchEvent(MotionEvent event) {
            return tonegen != null && tonegen.onTouch(event);
        }
    }

    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        int i, j, n;
        int[] white_keys = new int[] { 0, 2, 4, 5, 7, 9, 11, 12 };
        super.onCreate(savedInstanceState);
        myself = this;
        setContentView(R.layout.main);

        LinearLayout background = (LinearLayout)findViewById(R.id.background);
        LinearLayout keyboard = (LinearLayout)findViewById(R.id.keyboard);
        keys = new ConnectableButton[32];
        for (i = n = 0; i < 4; i++) {
            LinearLayout column = new LinearLayout(this);
            column.setOrientation(LinearLayout.VERTICAL);
            for (j = 0; j < 8; j++, n++) {
                ConnectableButton button = new ConnectableButton(this);
                button.setWidth(78);
                button.setHeight(60);
                //button.setText("" + n);
                if (j == 1 || j == 3 || j == 6) {
                    button.getBackground()
                        .setColorFilter(new LightingColorFilter(0xFF404040,
                                                                0xFF404040));
                } else {
                    button.getBackground()
                        .setColorFilter(new LightingColorFilter(0xFFFFFFFF,
                                                                0xFFFFFFFF));
                }
                keys[n] = button;
                column.addView(button, j);
            }
            keyboard.addView(column, i);
        }
        for (i = 0; i < pitches.length; i++)
            generators[i] = new ToneGenerator(i);
        final int OFFSET = 0;
        for (i = 0; i < 8; i++) {
            keys[i].setToneGenerator(generators[i + OFFSET]);
            keys[8 + i].setToneGenerator(generators[i + 7 + OFFSET]);
            keys[16 + i].setToneGenerator(generators[i + 12 + OFFSET]);
            keys[24 + i].setToneGenerator(generators[i + 19 + OFFSET]);
        }

        setVolumeControlStream(AudioManager.STREAM_MUSIC);
    }

    private MediaPlayer player;

    private class ToneGenerator {
        private int offset;
        public ToneGenerator(int pitchOffset) {
            offset = pitchOffset;
        }
        public boolean onTouch(MotionEvent event) {
            if (event.getAction() == MotionEvent.ACTION_DOWN) {
                if (player != null) {
                    player.stop();
                    player.reset();
                }
                player = MediaPlayer.create(myself, pitches[offset]);
                player.start();
                return true;
            }
            if (event.getAction() == MotionEvent.ACTION_UP) {
                player.stop();
                player.reset();
                return true;
            }
            return false;
        }
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        MenuInflater inflater = getMenuInflater();
        inflater.inflate(R.menu.foo, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        Intent intent;
        switch (item.getItemId()) {
            /*
            case R.id.settings_menu_item:
                intent = new Intent(this,
                                    TuningPicker.class);
                intent.putExtra("tuning", sprintTuning);
                startActivityForResult(intent, PICK_TUNING_REQUEST);
                return true;
            */
            case R.id.info_menu_item:
                showAbout();
                return true;
            default:
                return super.onOptionsItemSelected(item);
        }
    }

    /**
     * Show an about dialog that cites data sources.
     */
    protected void showAbout() {
        // Inflate the about message contents
        View messageView = getLayoutInflater().inflate(R.layout.about,
                                                       null, false);
        AlertDialog.Builder builder = new AlertDialog.Builder(this);
        builder.setIcon(R.drawable.icon);
        builder.setTitle(R.string.app_name);
        builder.setView(messageView);
        builder.create();
        builder.show();
    }

    /*
    protected void onActivityResult(int requestCode, int resultCode,
                                    Intent data) {
        if (requestCode == PICK_TUNING_REQUEST) {
            if (resultCode == RESULT_OK) {
                sprintTuning = data.getExtras().getInt("tuning");
            }
        }
    }
    */
}

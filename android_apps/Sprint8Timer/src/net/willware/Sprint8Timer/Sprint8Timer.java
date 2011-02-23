package net.willware.Sprint8Timer;

import android.app.Activity;
import android.app.AlertDialog;
import android.os.Bundle;
import android.widget.Button;
import android.widget.TextView;
import android.widget.RelativeLayout;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.WindowManager.LayoutParams;
import android.view.MenuInflater;
import android.view.Menu;
import android.view.MenuItem;
import android.os.Handler;
import android.content.Intent;
import android.content.SharedPreferences;
import android.net.Uri;
import android.media.MediaPlayer;
import android.media.AudioManager;

public class Sprint8Timer extends Activity implements Runnable
{
    private Button sprintButton;
    private TextView sprintNum;
    private TextView sprintTime;
    private long startTime;
    private int sprintIndex;
    private int sprintState;
    private Handler mHandler = new Handler();
    private RelativeLayout background;

    static final int PICK_DURATION_REQUEST = 0;

    public static final int DEFAULT_DURATION = 30;
    private static volatile int sprintDuration = DEFAULT_DURATION;

    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);
        sprintButton = (Button)findViewById(R.id.sprint);
        sprintButton.setOnClickListener(sprintButtonListener);
        sprintNum = (TextView)findViewById(R.id.sprintnum);
        sprintTime = (TextView)findViewById(R.id.showtime);
        background = (RelativeLayout)findViewById(R.id.background);
        getWindow().addFlags(LayoutParams.FLAG_KEEP_SCREEN_ON);
        SharedPreferences settings = getPreferences(MODE_PRIVATE);
        sprintDuration = settings.getInt("sprintDuration", DEFAULT_DURATION);
        setVolumeControlStream(AudioManager.STREAM_MUSIC);
        resetSprint();
    }

    @Override
    public void onStop() {
        super.onStop();
        SharedPreferences settings = getPreferences(MODE_PRIVATE);
        SharedPreferences.Editor editor = settings.edit();
        editor.putInt("sprintDuration", sprintDuration);
        editor.commit();
        mHandler.removeCallbacks(this);
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
            case R.id.settings_menu_item:
                intent = new Intent(this,
                                    DurationPicker.class);
                intent.putExtra("duration", sprintDuration);
                startActivityForResult(intent, PICK_DURATION_REQUEST);
                return true;
            case R.id.eula_menu_item:
                showEula();
                return true;
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

    protected void showEula() {
        // Inflate the eula message contents
        View messageView = getLayoutInflater().inflate(R.layout.eula,
                                                       null, false);
        AlertDialog.Builder builder = new AlertDialog.Builder(this);
        builder.setIcon(R.drawable.icon);
        builder.setTitle(R.string.app_name);
        builder.setView(messageView);
        builder.create();
        builder.show();
    }

    protected void onActivityResult(int requestCode, int resultCode,
                                    Intent data) {
        if (requestCode == PICK_DURATION_REQUEST) {
            if (resultCode == RESULT_OK) {
                sprintDuration = data.getExtras().getInt("duration");
            }
        }
    }

    private OnClickListener sprintButtonListener = new OnClickListener() {
            public void onClick(View v) {
                sprintIndex++;
                if (sprintIndex <= 8)
                    sprintState = 1;
                else
                    sprintState = 3;
                startTimer();
            }
        };

    private void startTimer() {
        mHandler.removeCallbacks(this);
        startTime = System.currentTimeMillis();
        run();
    }

    private void resetSprint() {
        sprintIndex = 0;
        sprintState = 0;   // before first sprint
        sprintNum.setText("Warm up");
        sprintTime.setText("00:00");
        background.setBackgroundResource(R.color.restColor);
        startTimer();
    }

    public void run() {
        mHandler.postDelayed(this, 1000);
        final String message;
        int bgcolor = R.color.restColor;
        int diff =
            ((int) (System.currentTimeMillis() - startTime)) / 1000;
        switch (sprintState) {
            case 0:
                message = "Warm up";
                break;
            case 1:
                if (diff >= sprintDuration) {
                    // a sprint is ending, play a tone, restart the timer
                    final Activity myself = this;
                    startTime = System.currentTimeMillis();
                    new Thread() {
                        public void run() {
                            MediaPlayer mp =
                                MediaPlayer.create(myself, R.raw.tone);
                            mp.start();
                        }
                    }.start();
                    if (sprintIndex >= 8) {
                        message = "Cool down";
                        sprintState = 3;
                    } else {
                        message = "Rest %d";
                        sprintState = 2;
                    }
                    diff = 0;
                } else {
                    message = "Sprint %d";
                    bgcolor = R.color.sprintColor;
                }
                break;
            case 2:
                message = "Rest %d";
                break;
            default:
                message = "Cool down";
                break;
        }
        background.setBackgroundResource(bgcolor);
        sprintNum.setText(String.format(message, sprintIndex));
        sprintTime.setText(String.format("%02d:%02d",
                                         (diff / 60) % 60, diff % 60));
    }
}

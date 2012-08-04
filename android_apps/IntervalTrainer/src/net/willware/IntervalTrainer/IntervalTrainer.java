package net.willware.IntervalTrainer;

import android.app.Activity;
import android.app.AlertDialog;
import android.os.Bundle;
import android.widget.Button;
import android.widget.TextView;
import android.widget.LinearLayout;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.WindowManager.LayoutParams;
import android.view.MenuInflater;
import android.view.Menu;
import android.view.MenuItem;
import android.os.Handler;
import android.content.Intent;
import android.content.SharedPreferences;
import android.media.MediaPlayer;
import android.media.AudioManager;

public class IntervalTrainer extends Activity implements Runnable
{
    private Button startButton;
    private Button resetButton;
    private TextView sprintNum;
    private TextView sprintTime;
    private long startTime;
    private int sprintIndex;
    private int sprintState;
    private Handler mHandler = new Handler();
    private LinearLayout background;
    private final IntervalTrainer myself = this;

    private static final int PICK_DURATION_REQUEST = 0;

    public static final int DEFAULT_SPRINT_DURATION = 30;
    private static volatile int sprintDuration = DEFAULT_SPRINT_DURATION;

    public static final int DEFAULT_REST_DURATION = 90;
    private static volatile int restDuration = DEFAULT_REST_DURATION;

    public static final int DEFAULT_NUM_SPRINTS = 8;
    private static volatile int numSprints = DEFAULT_NUM_SPRINTS;

    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);

        startButton = (Button)findViewById(R.id.start);
        startButton.setOnClickListener(buttonListener);
        resetButton = (Button)findViewById(R.id.reset);
        resetButton.setOnClickListener(buttonListener);

        sprintNum = (TextView)findViewById(R.id.sprintnum);
        sprintTime = (TextView)findViewById(R.id.showtime);
        background = (LinearLayout)findViewById(R.id.background);
        getWindow().addFlags(LayoutParams.FLAG_KEEP_SCREEN_ON);
        SharedPreferences settings = getPreferences(MODE_PRIVATE);
        sprintDuration =
            settings.getInt("sprintDuration", DEFAULT_SPRINT_DURATION);
        restDuration =
            settings.getInt("restDuration", DEFAULT_REST_DURATION);
        numSprints =
            settings.getInt("numSprints", DEFAULT_NUM_SPRINTS);
        setVolumeControlStream(AudioManager.STREAM_MUSIC);
    }

    @Override
    public void onResume() {
        super.onResume();
        resetSprint();
    }

    @Override
    public void onPause() {
        super.onPause();
        mHandler.removeCallbacks(this);
    }

    @Override
    public void onStop() {
        super.onStop();
        SharedPreferences settings = getPreferences(MODE_PRIVATE);
        SharedPreferences.Editor editor = settings.edit();
        editor.putInt("sprintDuration", sprintDuration);
        editor.putInt("restDuration", restDuration);
        editor.putInt("numSprints", numSprints);
        editor.commit();
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
                intent.putExtra("sprintDuration", sprintDuration);
                intent.putExtra("restDuration", restDuration);
                intent.putExtra("numSprints", numSprints);
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
                sprintDuration = data.getExtras().getInt("sprintDuration");
                restDuration = data.getExtras().getInt("restDuration");
                numSprints = data.getExtras().getInt("numSprints");
            }
        }
    }

    private OnClickListener buttonListener = new OnClickListener() {
            public void onClick(View v) {
                if (v == startButton) {
                    sprintState = 1;
                    startTimer();
                } else if (v == resetButton) {
                    resetSprint();
                }
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
        sprintNum.setText("Ready");
        sprintTime.setText("00:00");
        mHandler.removeCallbacks(myself);
        background.setBackgroundResource(R.color.restColor);
    }

    private void playTone() {
        new Thread() {
            public void run() {
                MediaPlayer mp = MediaPlayer.create(myself, R.raw.tone);
                mp.start();
            }
        }.start();
    }

    public void run() {
        mHandler.postDelayed(this, 1000);
        String message = "Ready";
        int bgcolor = R.color.restColor;
        int diff =
            ((int) (System.currentTimeMillis() - startTime)) / 1000;
        switch (sprintState) {
            case 0:
                message = "Ready";
                bgcolor = R.color.restColor;
                break;
            case 1:
                message = "Warm up";
                bgcolor = R.color.restColor;
                if (diff >= restDuration) {
                    // the first sprint is beginning
                    sprintIndex++;
                    playTone();
                    startTime = System.currentTimeMillis();
                    sprintState = 2;
                    message = "Sprint %d";
                    bgcolor = R.color.sprintColor;
                }
                break;
            case 2:
                message = "Sprint %d";
                bgcolor = R.color.sprintColor;
                if (diff >= sprintDuration) {
                    // a sprint is ending, play a tone, restart the timer
                    startTime = System.currentTimeMillis();
                    diff = 0;
                    playTone();
                    if (sprintIndex >= numSprints) {
                        message = "Cool down";
                    } else {
                        message = "Rest %d";
                    }
                    sprintState = 3;
                    bgcolor = R.color.restColor;
                }
                break;
            case 3:
                if (sprintIndex < numSprints)
                    message = "Rest %d";
                else
                    message = "Cool down";
                bgcolor = R.color.restColor;
                if (diff >= restDuration) {
                    // a sprint is beginning, or we've finished cooldown
                    startTime = System.currentTimeMillis();
                    diff = 0;
                    playTone();
                    sprintIndex++;
                    if (sprintIndex <= numSprints) {
                        sprintState = 2;
                        message = "Sprint %d";
                        bgcolor = R.color.sprintColor;
                    } else {
                        message = "Done";
                        bgcolor = R.color.restColor;
                        mHandler.removeCallbacks(myself);
                    }
                }
                break;
        }
        background.setBackgroundResource(bgcolor);
        sprintNum.setText(String.format(message, sprintIndex));
        sprintTime.setText(String.format("%02d:%02d",
                                         (diff / 60) % 60, diff % 60));
    }
}

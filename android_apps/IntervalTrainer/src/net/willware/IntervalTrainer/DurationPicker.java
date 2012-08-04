package net.willware.IntervalTrainer;

import android.app.Activity;
import android.os.Bundle;
import android.widget.Button;
import android.widget.TextView;
import android.view.View;
import android.view.View.OnTouchListener;
import android.view.MotionEvent;
import android.content.Intent;
import android.os.Handler;

public class DurationPicker extends Activity
{
    private TextView sprintTime;
    private int sprintDuration;
    private TextView restTime;
    private int restDuration;
    private TextView numSprints;
    private int numberOfSprints;

    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.number_picker);

        ((Button)findViewById(R.id.sprint_increment))
            .setOnTouchListener(new RepeatListener() {
                    public void addDelta() {
                        if (sprintDuration < 120)
                            sprintDuration++;
                        setIntentDuration("sprintDuration", sprintDuration);
                    }
                });
        ((Button)findViewById(R.id.sprint_decrement))
            .setOnTouchListener(new RepeatListener() {
                    public void addDelta() {
                        if (sprintDuration > 1)
                            sprintDuration--;
                        setIntentDuration("sprintDuration", sprintDuration);
                    }
                });
        sprintDuration =
            getIntent().getExtras()
            .getInt("sprintDuration",
                    IntervalTrainer.DEFAULT_SPRINT_DURATION);
        sprintTime = (TextView)findViewById(R.id.timepicker_sprint_input);

        ((Button)findViewById(R.id.rest_increment))
            .setOnTouchListener(new RepeatListener() {
                    public void addDelta() {
                        if (restDuration < 360)
                            restDuration++;
                        setIntentDuration("restDuration", restDuration);
                    }
                });
        ((Button)findViewById(R.id.rest_decrement))
            .setOnTouchListener(new RepeatListener() {
                    public void addDelta() {
                        if (restDuration > 1)
                            restDuration--;
                        setIntentDuration("restDuration", restDuration);
                    }
                });
        restDuration =
            getIntent().getExtras()
            .getInt("restDuration",
                    IntervalTrainer.DEFAULT_REST_DURATION);
        restTime = (TextView)findViewById(R.id.timepicker_rest_input);

        ((Button)findViewById(R.id.num_increment))
            .setOnTouchListener(new RepeatListener() {
                    public void addDelta() {
                        if (numberOfSprints < 30)
                            numberOfSprints++;
                        setIntentDuration("numSprints", numberOfSprints);
                    }
                });
        ((Button)findViewById(R.id.num_decrement))
            .setOnTouchListener(new RepeatListener() {
                    public void addDelta() {
                        if (numberOfSprints > 1)
                            numberOfSprints--;
                        setIntentDuration("numSprints", numberOfSprints);
                    }
                });
        numberOfSprints =
            getIntent().getExtras()
            .getInt("numSprints",
                    IntervalTrainer.DEFAULT_NUM_SPRINTS);
        numSprints = (TextView)findViewById(R.id.timepicker_numsprints_input);

        updateTimes();
    }

    private void setIntentDuration(String name, int value) {
        Intent intent = getIntent();
        intent.putExtra(name, value);
        setResult(RESULT_OK, intent);
        updateTimes();
    }

    private void updateTimes() {
        int minutes = sprintDuration / 60;
        int seconds = sprintDuration % 60;
        sprintTime.setText(String.format("%02d:%02d", minutes, seconds));
        minutes = restDuration / 60;
        seconds = restDuration % 60;
        restTime.setText(String.format("%02d:%02d", minutes, seconds));
        numSprints.setText("" + numberOfSprints);
    }

    private abstract class RepeatListener
        implements OnTouchListener, Runnable {
        private static final int period = 250;  // msecs
        private final Handler handler;
        public abstract void addDelta();
        public RepeatListener() {
            this.handler = new Handler();
        }
        public boolean onTouch(View view, MotionEvent motionEvent) {
            int action = motionEvent.getAction();
            if (action == MotionEvent.ACTION_DOWN) {
                addDelta();
                handler.removeCallbacks(this);
                handler.postDelayed(this, period);
            }
            else if (action == MotionEvent.ACTION_UP) {
                handler.removeCallbacks(this);
            }
            return false;
        }
        public void run() {
            addDelta();
            handler.postDelayed(this, period);
        }
    }
}

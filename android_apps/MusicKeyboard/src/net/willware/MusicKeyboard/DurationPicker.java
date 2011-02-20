package net.willware.MusicKeyboard;

import android.app.Activity;
import android.os.Bundle;
import android.widget.Button;
import android.widget.TextView;
import android.view.View;
import android.view.View.OnClickListener;
import android.content.Context;
import android.content.Intent;

public class DurationPicker extends Activity
{
    private Button incrButton;
    private Button decrButton;
    private TextView sprintTime;
    private int duration;

    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.number_picker);
        incrButton = (Button)findViewById(R.id.increment);
        incrButton.setOnClickListener(incrButtonListener);
        decrButton = (Button)findViewById(R.id.decrement);
        decrButton.setOnClickListener(decrButtonListener);
        duration =
            getIntent().getExtras().getInt("duration",
                                           MusicKeyboard.DEFAULT_DURATION);
        sprintTime = (TextView)findViewById(R.id.timepicker_input);
        updateSprintTime();
    }

    private void updateSprintTime() {
        final int minutes = duration / 60;
        final int seconds = duration % 60;
        sprintTime.setText(String.format("%02d:%02d", minutes, seconds));
    }

    private OnClickListener incrButtonListener = new OnClickListener() {
            public void onClick(View v) {
                if (duration < 120)
                    duration++;
                Intent intent = getIntent();
                intent.putExtra("duration", duration);
                setResult(RESULT_OK, intent);
                updateSprintTime();
            }
        };

    private OnClickListener decrButtonListener = new OnClickListener() {
            public void onClick(View v) {
                if (duration > 1)
                    duration--;
                Intent intent = getIntent();
                intent.putExtra("duration", duration);
                setResult(RESULT_OK, intent);
                updateSprintTime();
            }
        };
}

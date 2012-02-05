package net.willware.Dictaphone;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.os.Bundle;
import android.speech.RecognitionListener;
import android.speech.RecognizerIntent;
import android.speech.SpeechRecognizer;
import android.text.ClipboardManager;
import android.util.Log;
import android.view.View;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;

import java.lang.Math;
import java.util.ArrayList;
import java.util.List;

public class Dictaphone extends Activity
    implements View.OnClickListener, RecognitionListener
{
    private static final String TAG = "Dictaphone";

    private Button dictate_button;
    private EditText email_address;
    private Button send_button;
    private String results_so_far;
    private SharedPreferences preferences;

    private SpeechRecognizer recognizer;

    // ARGB, FF = opaque
    private static final int WHITE = 0xFFFFFFFF;
    private static final int RED = 0xFFFF4040;

    @Override
    public void onDestroy()
    {
        super.onDestroy();
        String email = email_address.getText().toString();
        SharedPreferences.Editor editor = preferences.edit();
        editor.putString("emailAddress", email);
        editor.commit();
        recognizer.destroy();
    }

    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        preferences = getPreferences(MODE_PRIVATE);
        results_so_far = "";
        setContentView(R.layout.main);
        dictate_button = (Button)findViewById(R.id.dictate_button);
        send_button = (Button)findViewById(R.id.send_button);
        email_address = (EditText)findViewById(R.id.email_address);
        dictate_button.setOnClickListener(this);
        send_button.setOnClickListener(this);

        Context ctx = getApplicationContext();

        if (SpeechRecognizer.isRecognitionAvailable(ctx)) {
            recognizer =
                SpeechRecognizer.createSpeechRecognizer(ctx);
            recognizer.setRecognitionListener(this);
        } else {
            dictate_button.setEnabled(false);
            dictate_button.setText("Recognizer??");
            send_button.setEnabled(false);
        }

        email_address.setText(preferences.getString("emailAddress", ""));
    }

    public void onClick(View v) {
        String email = email_address.getText().toString();
        if (v == dictate_button) {
            if ("Dictate".equals(dictate_button.getText())) {
                Intent intent = new Intent(RecognizerIntent.ACTION_RECOGNIZE_SPEECH);
                intent.putExtra(RecognizerIntent.EXTRA_LANGUAGE_MODEL,
                                RecognizerIntent.LANGUAGE_MODEL_FREE_FORM);
                intent.putExtra(RecognizerIntent.EXTRA_PROMPT, "Dictaphone");
                intent.putExtra(RecognizerIntent.EXTRA_CALLING_PACKAGE, "net.willware.Dictaphone");
                recognizer.startListening(intent);
            } else {
                recognizer.stopListening();
                dictate_button.setText("Dictate");
                dictate_button.setBackgroundColor(WHITE);
            }
        } else if (v == send_button) {
            Intent intent = new Intent(Intent.ACTION_SEND);
            intent.setType("message/rfc822");
            intent.putExtra(Intent.EXTRA_EMAIL,
                            new String[]{ email_address.getText().toString() });
            intent.putExtra(Intent.EXTRA_SUBJECT,
                            "Android phone voice recording");
            intent.putExtra(Intent.EXTRA_TEXT, results_so_far);
            results_so_far = "";
            try {
                startActivity(Intent.createChooser(intent, "Send mail..."));
            } catch (android.content.ActivityNotFoundException ex) {
                Toast.makeText(this,
                               "There are no email clients installed.",
                               Toast.LENGTH_SHORT).show();
            }
        }
    }

    /* Implement the speech recognition listener interface */
    public void onBeginningOfSpeech() {
        Log.i(TAG, "onBeginningOfSpeech");
    }

    public void onBufferReceived(byte[] buffer) {
    }

    public void onEndOfSpeech() {
        Log.i(TAG, "onEndOfSpeech");
    }

    public void onError(int error) {
        String s;
        switch (error) {
            default:
                s = "????";
                break;
            case SpeechRecognizer.ERROR_AUDIO:
                s = "Audio recording error";
                break;
            case SpeechRecognizer.ERROR_CLIENT:
                s = "Other client side errors";
                break;
            case SpeechRecognizer.ERROR_INSUFFICIENT_PERMISSIONS:
                s = "Insufficient permission";
                break;
            case SpeechRecognizer.ERROR_NETWORK:
                s = "Other network related errors";
                break;
            case SpeechRecognizer.ERROR_NETWORK_TIMEOUT:
                s = "Network operation timed out";
                break;
            case SpeechRecognizer.ERROR_NO_MATCH:
                s = "No recognition result matched";
                break;
            case SpeechRecognizer.ERROR_RECOGNIZER_BUSY:
                s = "RecognitionService busy";
                break;
            case SpeechRecognizer.ERROR_SERVER:
                s = "Server sends error status";
                break;
            case SpeechRecognizer.ERROR_SPEECH_TIMEOUT:
                s = "No speech input";
                break;
        }
        Log.i(TAG, "onError: " + s);
        Toast.makeText(this,
                       "Speech recognition problem: " + s,
                       Toast.LENGTH_SHORT).show();
        runOnUiThread(new Runnable() {
                public void run() {
                    dictate_button.setText("Dictate");
                    dictate_button.setBackgroundColor(WHITE);
                }
            });
    }

    public void onEvent(int eventType, Bundle params) {
    }

    public void onPartialResults(Bundle partialResults) {
    }

    public void onReadyForSpeech(Bundle params) {
        Log.i(TAG, "onReadyForSpeech");
        runOnUiThread(new Runnable() {
                public void run() {
                    dictate_button.setText("Press to stop");
                    dictate_button.setBackgroundColor(RED);
                }
            });
    }

    public void onResults(Bundle results) {
        String s = disambiguate
            (results.getStringArrayList(SpeechRecognizer.RESULTS_RECOGNITION));
        Log.i(TAG, "onResults: " + s);
        results_so_far += s + "\n";
    }

    public void onRmsChanged(float rmsdB) {
    }

    private String disambiguate(ArrayList<String> matches) {
        int index = 0;
        ArrayList<String[]> wordlists = new ArrayList<String[]>();
        ArrayList<String> result = new ArrayList<String>();
        for (int i = 0; i < matches.size(); i++) {
            wordlists.add(matches.get(i).split("\\s+"));
        }

        String[] w0 = wordlists.get(0);
        while (index < w0.length) {
            boolean allSame = true;
            for (int i = 1; i < wordlists.size(); i++) {
                String[] wi = wordlists.get(i);
                if (index < wi.length && !wi[index].equals(w0[index])) {
                    allSame = false;
                    break;
                }
            }
            if (allSame)
                result.add(w0[index]);
            else {
                int i = 1;
                while (i < wordlists.size()) {
                    int m = 0, n = 0;
                    String[] wi = wordlists.get(i);
                    boolean ok = false;
                    for (int j = 0; j < 21; j++) {
                        int p = (int)(0.5 + Math.sqrt(2.0 * j + 0.25));
                        m = i - p * (p - 1) / 2;
                        n = p - m - 1;
                        if (index + m >= 0 &&
                            index + n >= 0 &&
                            index + m + 1 < w0.length &&
                            index + n + 1 < wi.length)
                            if (w0[index+m].equals(wi[index+n]) &&
                                w0[index+m+1].equals(wi[index+n+1])) {
                                ok = true;
                                break;
                            }
                    }
                    if (ok) {
                        if (m == 0 && n == 0) {
                            // do nothing
                        } else if (m == 1) {
                            String[] newWi = new String[wi.length + 1 - n];
                            for (int j = 0; j <= index; j++)
                                newWi[j] = wi[j];
                            for (int j = index + n; j < wi.length; j++)
                                newWi[j+1-n] = wi[j];
                            wordlists.set(i, newWi);
                        } else {
                            wordlists.remove(i);
                        }
                    }
                    i++;
                }
                result.add(w0[index] + "?");
            }
            index++;
        }

        StringBuilder retval = new StringBuilder();
        for (String s : result) {
            retval.append(" ");
            retval.append(s);
        }
        if (retval.length() >= 1)
            return retval.substring(1);
        else
            return "";
    }
}

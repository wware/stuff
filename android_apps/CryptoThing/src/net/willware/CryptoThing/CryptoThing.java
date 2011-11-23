package net.willware.CryptoThing;

import android.app.Activity;
import android.content.Context;
import android.text.ClipboardManager;
import android.os.Bundle;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TextView;
import android.view.View;

public class CryptoThing extends Activity implements View.OnClickListener
{
    private EditText password_field;
    private EditText input_window;
    private TextView output_window;
    private Button encrypt_button;
    private Button decrypt_button;
    private Button paste_button;
    private Button copy_button;
    private Button clear_button;

    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);
        password_field = (EditText)findViewById(R.id.password_field);
        input_window = (EditText)findViewById(R.id.input_window);
        output_window = (TextView)findViewById(R.id.output_window);
        encrypt_button = (Button)findViewById(R.id.encrypt_button);
        decrypt_button = (Button)findViewById(R.id.decrypt_button);
        paste_button = (Button)findViewById(R.id.paste_button);
        copy_button = (Button)findViewById(R.id.copy_button);
        clear_button = (Button)findViewById(R.id.clear_button);
        encrypt_button.setOnClickListener(this);
        decrypt_button.setOnClickListener(this);
        paste_button.setOnClickListener(this);
        copy_button.setOnClickListener(this);
        clear_button.setOnClickListener(this);
    }

    public void onClick(View v) {
        ClipboardManager clipboard =
            (ClipboardManager) getSystemService(Context.CLIPBOARD_SERVICE);
        String key = password_field.getText().toString();
        String input = input_window.getText().toString();
        String output = output_window.getText().toString();
        if (v == encrypt_button) {
            output = RC4.cipherSaberEncrypt(key, input);
        }
        else if (v == decrypt_button) {
            output = RC4.cipherSaberDecrypt(key, input);
        }
        else if (v == clear_button) {
            input_window.setText("");
        }
        else if (v == paste_button) {
            input_window.setText(clipboard.getText());
        }
        else if (v == copy_button) {
            clipboard.setText(output);
        }
        output_window.setText(output);
    }
}

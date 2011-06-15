package net.willware.locationsender;

import android.app.Activity;
import android.os.Bundle;
import android.content.Context;
import android.location.LocationManager;
import android.location.Criteria;
import android.location.Location;
import android.location.LocationListener;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TextView;
import android.view.View;
import android.view.View.OnClickListener;

public class LocationSender extends Activity implements LocationListener {

	private Criteria fineCriteria;
	private Criteria coarseCriteria;
	private LocationManager lm;
	private boolean fineUpdates = false;

	private static int UPDATE_MINTIME = 10000;  // 10 seconds
	private static float FINE_UPDATE_MINDISTANCE = 15.0f;  // 15 meters -> ~50 feet
	private static float COARSE_UPDATE_MINDISTANCE = 100.0f;  // 100 meters -> ~300 feet

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);

        lm = (LocationManager) getSystemService(Context.LOCATION_SERVICE);
		fineCriteria = new Criteria();
        fineCriteria.setAccuracy(Criteria.ACCURACY_FINE);
		coarseCriteria = new Criteria();
        coarseCriteria.setAccuracy(Criteria.ACCURACY_COARSE);
		fineUpdates = false;

        ((Button) findViewById(R.id.getfine)).setOnClickListener(new OnClickListener() {
            public void onClick(View v) {
            	fineUpdates = true;
            	String fineProvider = lm.getBestProvider(fineCriteria, true);
            	lm.removeUpdates(LocationSender.this);
            	lm.requestLocationUpdates(fineProvider,
            			                  UPDATE_MINTIME,
            			                  FINE_UPDATE_MINDISTANCE,
            			                  LocationSender.this);
            }
        });
        ((Button) findViewById(R.id.getcoarse)).setOnClickListener(new OnClickListener() {
            public void onClick(View v) {
            	fineUpdates = false;
            	String coarseProvider = lm.getBestProvider(coarseCriteria, true);
            	lm.removeUpdates(LocationSender.this);
            	Location loc = lm.getLastKnownLocation(coarseProvider);
            	if (loc != null) {
            		LocationSender.this.onLocationChanged(loc);
            		return;
            	}
        		lm.requestLocationUpdates(coarseProvider,
        				                  UPDATE_MINTIME,
        				                  COARSE_UPDATE_MINDISTANCE,
        				                  LocationSender.this);
            }
        });
    }

    @Override
    public void onPause() {
    	super.onPause();
    	lm.removeUpdates(this);
    	((EditText) findViewById(R.id.message)).setText("", TextView.BufferType.EDITABLE);
    	fineUpdates = false;
    }

    @Override
    public void onDestroy() {
    	super.onDestroy();
    	lm.removeUpdates(this);
    }

    @Override
    public void onLocationChanged(Location location) {
        // http://maps.google.com/?ll=42.33991,-71.393623
        // Should I bit.ly this?
    	String message, latlon;
    	if (fineUpdates) {
    		latlon = String.format("http://maps.google.com/?q=%.6f,%.6f",
    							   location.getLatitude(),
    							   location.getLongitude());
    		message = getString(R.string.messagefine);
    	} else {
    		latlon = String.format("http://maps.google.com/?q=%.4f,%.4f",
    							   location.getLatitude(),
    							   location.getLongitude());
    		message = getString(R.string.messagecoarse);
    	}
        latlon = message + " " + latlon;
    	((EditText) findViewById(R.id.message)).setText(latlon, TextView.BufferType.EDITABLE);
    	lm.removeUpdates(this);
    }

    @Override
    public void onProviderDisabled(String provider) {
    	// do nothing for now
    }

    @Override
    public void onProviderEnabled(String provider) {
    	// do nothing for now
    }

    @Override
    public void onStatusChanged(String provider, int status, Bundle extras) {
    	// do nothing for now
    }
}

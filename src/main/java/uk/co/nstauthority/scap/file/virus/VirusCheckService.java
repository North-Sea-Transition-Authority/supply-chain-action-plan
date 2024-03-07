package uk.co.nstauthority.scap.file.virus;

import java.io.IOException;
import java.io.InputStream;

public interface VirusCheckService {

  boolean hasVirus(InputStream inputStream) throws IOException;

}
package com.gdn.micro.graphics.model;

import java.io.IOException;
import java.io.InputStream;

import org.apache.commons.lang3.StringUtils;
import org.im4java.process.OutputConsumer;

/**
 * Created by Yudhi K. Surtan on 11/28/2015.
 */
public class ChmodTarget implements OutputConsumer, Cloneable {
  private final String targetFile;
  private final int chmod;

  public ChmodTarget(String targetFile, int chmod) {
    this.targetFile =
        StringUtils.remove(StringUtils.remove(StringUtils.remove(targetFile, ';'), '&'), '|');
    this.chmod = chmod;
  }

  @Override
  public void consumeOutput(InputStream inputStream) throws IOException {
    Runtime.getRuntime().exec("chmod " + this.chmod + " " + this.targetFile);
  }

}

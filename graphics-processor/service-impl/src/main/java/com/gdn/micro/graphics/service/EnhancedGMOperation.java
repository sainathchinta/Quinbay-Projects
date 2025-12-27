package com.gdn.micro.graphics.service;

import org.gm4java.im4java.GMOperation;
import org.im4java.core.Operation;

/**
 * Created by hidayat.febiansyah on 6/3/2016.
 */
public class EnhancedGMOperation extends GMOperation {
  public EnhancedGMOperation() {
    super();
  }

  public Operation format(String formatParam) {
    return this.addRawArgs("-format", formatParam);
  }

}

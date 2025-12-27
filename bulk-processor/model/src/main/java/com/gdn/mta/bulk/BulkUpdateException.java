package com.gdn.mta.bulk;

import lombok.AllArgsConstructor;
import lombok.Data;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Created by tonny.kurniawan on 11/22/2018
 */
@Data
@AllArgsConstructor
public class BulkUpdateException extends Exception {
  private static final long serialVersionUID = -578336643416362950L;
  private String errorCode;
  private String errorMessage;
}

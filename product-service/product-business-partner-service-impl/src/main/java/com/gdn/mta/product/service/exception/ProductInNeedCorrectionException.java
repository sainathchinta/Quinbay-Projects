package com.gdn.mta.product.service.exception;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class ProductInNeedCorrectionException extends RuntimeException {
  private static final long serialVersionUID = -1568681981639506972L;
  private String errorCode;
  private String errorMessage;
}

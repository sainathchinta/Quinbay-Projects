package com.gdn.mta.product.service.exception;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class ProductInReviewException extends RuntimeException {
  private static final long serialVersionUID = 8556508951136832547L;
  private String errorCode;
  private String errorMessage;
}

package com.gdn.mta.product.service.exception;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class ProductRejectedException extends RuntimeException {
  private static final long serialVersionUID = 835620010303972871L;
  private String errorCode;
  private String errorMessage;
}

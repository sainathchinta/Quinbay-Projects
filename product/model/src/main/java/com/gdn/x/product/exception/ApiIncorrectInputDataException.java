package com.gdn.x.product.exception;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class ApiIncorrectInputDataException  extends RuntimeException{
  private static final long serialVersionUID = -3647750388733623282L;
  public String errorMessage;
  public String errorCode;
}
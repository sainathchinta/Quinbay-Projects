package com.gdn.x.productcategorybase.outbound.model;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@AllArgsConstructor
@NoArgsConstructor
@Data
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class SingleBaseResponse<T> implements Serializable {
  private static final long serialVersionUID = 2911739849736715458L;
  private boolean success;
  private String errorCode;
  private String errorMessage;
  private T value;
}

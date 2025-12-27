package com.gdn.x.product.rest.web.model.util;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;

public class GdnRestSimpleResponse<T> extends GdnBaseRestResponse{

  private static final long serialVersionUID = -5916478700524311835L;
  private T value;

  public GdnRestSimpleResponse() {}

  public GdnRestSimpleResponse(String requestId, T value) {
    super(requestId);
    this.value = value;
  }

  public GdnRestSimpleResponse(String errorMessage, String errorCode, boolean success, String requestId, T value) {
    super(errorMessage, errorCode, success, requestId);
    this.value = value;
  }

  public T getValue() {
    return value;
  }

  public void setValue(T value) {
    this.value = value;
  }

  @Override
  public String toString() {
    return String
        .format(
            "GdnRestSimpleResponse [value=%s, getErrorCode()=%s, getErrorMessage()=%s, isSuccess()=%s, toString()=%s, getRequestId()=%s, getClass()=%s, hashCode()=%s]",
            value, getErrorCode(), getErrorMessage(), isSuccess(), super.toString(), getRequestId(), getClass(),
            hashCode());
  }
}

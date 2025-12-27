package com.gdn.x.productcategorybase.dto.response;


import com.gdn.common.web.base.BaseResponse;
import org.apache.commons.lang3.builder.ToStringBuilder;

/**
 * Created by Kesha on 09/06/16.
 */
public class SingleObjectResponse<T> extends BaseResponse {

  private static final long serialVersionUID = 2911739849736715457L;
  private T value;

  public SingleObjectResponse() {
  }

  public SingleObjectResponse(T value) {
    this.value = value;
  }

  public T getValue() {
    return value;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this)
        .append("value", value)
        .toString();
  }
}


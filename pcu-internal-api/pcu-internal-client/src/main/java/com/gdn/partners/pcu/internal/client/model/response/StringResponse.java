package com.gdn.partners.pcu.internal.client.model.response;

import com.gdn.common.web.base.BaseResponse;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;

@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
public class StringResponse  extends BaseResponse implements Serializable {
  @Serial
  private static final long serialVersionUID = -4800585027788362902L;
  private String value;
}

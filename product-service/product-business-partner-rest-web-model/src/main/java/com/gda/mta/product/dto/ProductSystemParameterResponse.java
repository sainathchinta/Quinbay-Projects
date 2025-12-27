package com.gda.mta.product.dto;

import java.io.Serializable;

import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ProductSystemParameterResponse extends BaseResponse implements Serializable {

  private String variable;
  private String value;
  private String description;
  private boolean showOnUI;

}

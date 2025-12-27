package com.gda.mta.product.dto;

import java.io.Serializable;

import com.gdn.common.web.base.BaseRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ProductSystemParameterRequest extends BaseRequest implements Serializable {

  private static final long serialVersionUID = 2583318644504866906L;
  private String variable;
  private String value;
  private String description;
  private boolean showOnUI;
}

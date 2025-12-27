package com.gdn.x.productcategorybase.dto.request;

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
public class SystemParameterRequest extends BaseRequest implements Serializable {

  private static final long serialVersionUID = 3184148888732419206L;
  private String variable;
  private String value;
  private String description;

}

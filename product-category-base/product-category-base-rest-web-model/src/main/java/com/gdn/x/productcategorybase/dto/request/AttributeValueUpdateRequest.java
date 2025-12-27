package com.gdn.x.productcategorybase.dto.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class AttributeValueUpdateRequest extends BaseDTORequest {

  private static final long serialVersionUID = -698261854173437051L;

  private String id;
  private String allowedAttributeCode;
  private String value;
  private String valueEn;
  private String valueType;
  private Integer sequence;

}

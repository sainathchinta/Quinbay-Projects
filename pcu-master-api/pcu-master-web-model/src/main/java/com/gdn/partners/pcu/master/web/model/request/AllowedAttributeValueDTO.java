package com.gdn.partners.pcu.master.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author BhagwatiMalav - created on 02/11/18
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class AllowedAttributeValueDTO extends BaseDTORequest {

  private static final long serialVersionUID = 4401370278831327487L;

  private String allowedAttributeCode;
  private String value;
  private String valueType;
  private Integer sequence;
}

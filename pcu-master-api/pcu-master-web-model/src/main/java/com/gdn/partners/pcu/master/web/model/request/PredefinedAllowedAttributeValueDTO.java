package com.gdn.partners.pcu.master.web.model.request;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author BhagwatiMalav - created on 02/11/18
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class PredefinedAllowedAttributeValueDTO extends BaseDTORequest{

  private static final long serialVersionUID = 1498590183490887280L;

  private String predefinedAllowedAttributeCode;
  private String value;
  private String valueEn;
  private Integer sequence;

  public PredefinedAllowedAttributeValueDTO(String predefinedAllowedAttributeCode, String value,
      Integer sequence) {
    this.predefinedAllowedAttributeCode = predefinedAllowedAttributeCode;
    this.value = value;
    this.sequence = sequence;
  }
}

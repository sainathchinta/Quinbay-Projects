package com.gdn.partners.pcu.master.model.attribute;


import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author Pradeep Reddy
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class AttributeValueUpdateModel {

  private String id;
  private String allowedAttributeCode;
  @NotNull(message = "Value should not be null")
  private String value;
  private String valueEn;
  private Integer sequence;
  private String valueType;

}

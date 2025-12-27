
package com.gdn.partners.pcu.external.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;


@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3AttributeResponse extends BaseResponse {
  private static final long serialVersionUID = -5260565716852218481L;
  private String attributeCode;
  private String attributeType;
  private List<String> values;
  private String valueType;
  private boolean sizeAttribute;
  private Boolean skuValue;
  private String attributeName;
  private String itemSku;
  private boolean variantCreation;
  private boolean mandatory;
  private boolean basicView;
  private boolean extractedValue;
  private boolean dsExtraction;
  private boolean hideFromSeller;

  @Override
  public String toString() {
    return "ProductLevel3AttributeResponse{" + "attributeCode='" + attributeCode + '\'' + ", attributeType='"
        + attributeType + '\'' + ", values=" + values + ", valueType='" + valueType + '\'' + ", sizeAttribute="
        + sizeAttribute + ", skuValue=" + skuValue + ", attributeName='" + attributeName + '\'' + ", itemSku='"
        + itemSku + '\'' + ", variantCreation=" + variantCreation + ", mandatory=" + mandatory + ", basicView="
        + basicView + ", extractedValue=" + extractedValue + ", dsExtraction=" + dsExtraction + ", hideFromSeller="
        + hideFromSeller + '}';
  }
}

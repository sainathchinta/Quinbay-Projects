package com.gdn.partners.pcu.master.web.model.request;

import java.util.ArrayList;
import java.util.List;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author BhagwatiMalav - created on 02/11/18
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterAttributeDTO extends BaseDTORequest{

  private static final long serialVersionUID = -5360017120267513603L;

  private String name;
  private String nameEnglish;
  private String dsAttributeName;
  private byte[] descriptionEnglish;
  private AttributeSortTypeDTO sortType;
  private String attributeCode;
  private AttributeTypeDTO attributeType;
  private boolean searchAble = false;
  private boolean mandatory;
  private byte[] description;
  private boolean skuValue = false;
  private boolean sizeAttribute;
  private List<String> valueTypes = new ArrayList<>();
  private String attributeImageUrl;
  private String example;
  private boolean isBasicView = true;
  private List<AllowedAttributeValueDTO> allowedAttributeValues = new ArrayList<>();
  private List<PredefinedAllowedAttributeValueDTO> predefinedAllowedAttributeValues = new ArrayList<>();
  private List<DimensionMappingDTO> dimensionMapping = new ArrayList<>();
  private boolean variantCreation = false;
  private boolean mustShowOnCustomerSide = false;
  private boolean dsExtraction = false;
  private boolean hideForSeller = false;
  private boolean hideForCustomer = false;
  private boolean multiLanguage = false;
}

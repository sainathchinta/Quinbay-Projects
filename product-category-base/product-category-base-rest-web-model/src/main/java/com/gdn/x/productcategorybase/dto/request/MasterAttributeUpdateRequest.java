package com.gdn.x.productcategorybase.dto.request;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterAttributeUpdateRequest extends BaseDTORequest{

  private static final long serialVersionUID = 4181506525832479536L;

  private AttributeSortTypeRequest sortType;

  private List<AttributeValueUpdateRequest> attributeValues = new ArrayList<>();

  private List<AttributeValueUpdateRequest> addedAttributeValues = new ArrayList<>();

  private List<AttributeValueUpdateRequest> deletedAttributeValues = new ArrayList<>();

}

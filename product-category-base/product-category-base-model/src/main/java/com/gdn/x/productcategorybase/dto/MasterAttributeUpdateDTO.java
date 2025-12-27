package com.gdn.x.productcategorybase.dto;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.gdn.x.productcategorybase.AttributeSortType;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class MasterAttributeUpdateDTO {

  private AttributeSortType sortType;

  private List<AttributeValueUpdateDTO> attributeValues = new ArrayList<>();

  private List<AttributeValueUpdateDTO> addedAttributeValues = new ArrayList<>();

  private List<AttributeValueUpdateDTO> deletedAttributeValues = new ArrayList<>();

  private Date updatedDate;

  private String updatedBy;

}

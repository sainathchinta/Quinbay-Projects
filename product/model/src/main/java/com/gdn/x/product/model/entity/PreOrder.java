package com.gdn.x.product.model.entity;

import java.util.Date;

import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.x.product.enums.ProductFieldNames;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class PreOrder implements GdnBaseEmbedded {
  private static final long serialVersionUID = -2463894438090551168L;

  @Field(value = ProductFieldNames.IS_PREORDER)
  private Boolean isPreOrder;

  @Field(value = ProductFieldNames.PREORDER_TYPE)
  private String preOrderType;

  @Field(value = ProductFieldNames.PREORDER_VALUE)
  private Integer preOrderValue;

  @Field(value = ProductFieldNames.PREORDER_DATE)
  private Date preOrderDate;

}

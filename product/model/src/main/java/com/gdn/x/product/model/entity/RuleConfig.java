package com.gdn.x.product.model.entity;

import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.x.product.enums.ProductScoreRuleFieldNames;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class RuleConfig implements GdnBaseEmbedded {
  @Field(value = ProductScoreRuleFieldNames.OPERATOR)
  private String operator;

  @Field(value = ProductScoreRuleFieldNames.VALUE)
  private int value;

  @Field(value = ProductScoreRuleFieldNames.SCORE)
  private double score;
}

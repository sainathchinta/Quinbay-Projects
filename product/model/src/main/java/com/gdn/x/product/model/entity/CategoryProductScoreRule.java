package com.gdn.x.product.model.entity;

import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.x.product.enums.ProductScoreRuleFieldNames;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@Document(collection = CategoryProductScoreRule.DOCUMENT_NAME)
public class CategoryProductScoreRule extends GdnBaseMongoEntity {
  private static final long serialVersionUID = 1L;
  public static final String DOCUMENT_NAME = "prd_category_product_score_rule";

  @Field(value = ProductScoreRuleFieldNames.CATGEORY_CODE)
  @Indexed
  private String categoryCode;

  @Field(value = ProductScoreRuleFieldNames.RULE_NAME)
  private String ruleName;

  @Field(value = ProductScoreRuleFieldNames.RULE_TYPE)
  private String ruleType;

  @Field(value = ProductScoreRuleFieldNames.MAX_SCORE)
  private int maxScore;

  @Field(value = ProductScoreRuleFieldNames.RULE_CONFIG)
  private List<RuleConfig> ruleConfig;
}

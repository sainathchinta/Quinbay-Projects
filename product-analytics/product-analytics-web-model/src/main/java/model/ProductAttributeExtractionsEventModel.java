package model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class ProductAttributeExtractionsEventModel extends GdnBaseDomainEventModel {
  private String storeId;
  private String productId;
  private String productSku;
  private Date addedDate;
  private List<AttributeValueExtractionsEventModel> attributeValueExtractions;
}
package com.gdn.mta.product.converter;

import java.util.ArrayList;
import java.util.Objects;

import org.apache.commons.lang3.StringUtils;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.stereotype.Component;

import com.gda.mta.product.dto.ProductLevel3SummaryDetailsRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryMinifiedRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilter;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilterDetails;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3ItemSearchRequest;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3ItemSearch;

@Component
public class ProductLevel3RequestConverterBean implements ProductLevel3RequestConverter {

  @Override
  public ProductLevel3SummaryFilter convertProductLevel3SummaryRequestToProductLevel3SummaryFilter(
      ProductLevel3SummaryRequest request) {
    ProductLevel3SummaryFilter filter = new ProductLevel3SummaryFilter();
    if (request != null) {
      BeanUtils.copyProperties(request, filter);
      if (StringUtils.isNotBlank(filter.getGdnSku())) {
        if (Objects.isNull(filter.getItemSkus())) {
          filter.setItemSkus(new ArrayList<>());
        }
        filter.getItemSkus().add(filter.getGdnSku());
      }
      if (request.isIgnoreArchive()) {
        filter.setArchived(null);
      }
    }
    return filter;
  }

  @Override
  public ProductLevel3SummaryFilter convertProductLevel3SummaryMinifiedRequestToProductLevel3SummaryFilter(
      ProductLevel3SummaryMinifiedRequest request) {
    ProductLevel3SummaryFilter filter = new ProductLevel3SummaryFilter();
    if (request != null) {
      filter.setDisplayable(request.getDisplay());
      filter.setBuyable(request.getBuyable());
      filter.setArchived(request.getArchived());
      filter.setInventoryFilter(request.getInventoryFilter());
    }
    return filter;
  }

  @Override
  public ProductLevel3ItemSearch convertProductLevel3ItemSearchRequestToProductLevel3ItemSearch(
      ProductLevel3ItemSearchRequest request) {
    if (request == null) {
      return new ProductLevel3ItemSearch();
    }
    return ProductLevel3ItemSearch.builder().itemNameKeyword(request.getItemNameKeyword())
        .itemSkuKeyword(request.getItemSkuKeyword()).buyable(request.getBuyable())
        .isArchived(request.getIsArchived())
        .includeAllTradingProduct(request.isIncludeAllTradingProduct()).build();
  }

  @Override
  public ProductLevel3SummaryFilterDetails convertProductLevel3SummaryRequestToProductLevel3SummaryDetailsFilter(
      ProductLevel3SummaryDetailsRequest request) {
    return ProductLevel3SummaryFilterDetails.builder().businessPartnerCode(request.getBusinessPartnerCode())
        .productSku(request.getProductSku()).itemSku(request.getItemSku()).isNeedCorrection(request.isNeedCorrection())
        .productCode(request.getProductCode()).build();
  }
}

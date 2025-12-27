package com.gdn.partners.pbp.service.productlevel3;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.ProductLevel3Logistics;
import com.gdn.mta.product.service.ErrorMessages;
import com.gdn.partners.pbp.outbound.sellerLogistics.SellerLogisticsOutbound;
import com.gdn.seller.logistics.web.model.request.LogisticProductSkuLevel;
import com.gdn.seller.logistics.web.model.request.SaveSkuLogisticProductRequest;
import com.gdn.seller.logistics.web.model.response.GetSkuLogisticProductResponse;

@Service
public class ProductLevel3LogisticsServiceBean implements ProductLevel3LogisticsService {

  private static final Logger LOGGER =
      LoggerFactory.getLogger(ProductLevel3LogisticsServiceBean.class);

  @Autowired
  private SellerLogisticsOutbound sellerLogisticsOutbound;

  @Autowired
  private ProductLevel3Converter productLevel3Converter;

  @Override
  public List<ProductLevel3Logistics> findLogisticsByItemSku(String itemSku, String merchantCode,
      String merchantDeliveryType) {
    try {
      List<GetSkuLogisticProductResponse> getSkuLogisticProductResponses =
          sellerLogisticsOutbound.getSkuLogistics(itemSku, merchantCode, merchantDeliveryType);
      if (CollectionUtils.isNotEmpty(getSkuLogisticProductResponses)) {
        return productLevel3Converter
            .convertLogisticDetailsToItemLevel3Logistics(getSkuLogisticProductResponses);
      }
    } catch (Exception ex) {
      LOGGER.error("Error when getting logistic detail for itemSku : {} Exception : {}", itemSku,
          ex.getStackTrace());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          ErrorMessages.LOGISTICS_GET_FAILED);
    }
    return new ArrayList<>();
  }

  @Override
  public Boolean saveLogisticsByItemSku(List<String> itemSkus, String merchantCode,
      List<ProductLevel3Logistics> logistics, boolean isActive) {
    try {
      if (CollectionUtils.isNotEmpty(itemSkus)) {
        List<String> distinctItemSku = itemSkus.stream().distinct().collect(Collectors.toList());
        SaveSkuLogisticProductRequest request =
          SaveSkuLogisticProductRequest.builder().itemSkus(distinctItemSku)
            .merchantCode(merchantCode).logisticProductCodes(new ArrayList<>()).build();
        if (CollectionUtils.isNotEmpty(logistics)) {
          for (ProductLevel3Logistics level3Logistics : logistics) {
            request.getLogisticProductCodes().add(LogisticProductSkuLevel.builder()
              .logisticProductCode(level3Logistics.getLogisticProductCode())
              .isSelected(level3Logistics.isSelected()).build());
          }
          sellerLogisticsOutbound.saveSkuLogistics(request, isActive);
        }
      }
    } catch (Exception ex) {
      LOGGER.error(
          "Error when saving logistic detail for itemSkus : {} logistics : {} isActive : {} Exception : {}",
          itemSkus, logistics, isActive, ex.getStackTrace());
      return false;
    }
    return true;
  }
}

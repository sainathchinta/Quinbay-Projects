package com.gdn.x.product.service.event.listener;

import java.util.List;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.enums.Constants;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.ProductScoreUpdateDomainEventModel;

@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.x.productcategorybase.product.score.listener.enabled",
                       havingValue = "true")
public class ProductScoreUpdateListener {

  @Autowired
  private ProductService productService;

  @Autowired
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = DomainEventName.PRODUCT_SCORE_UPDATE_EVENT_NAME, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Consume product score event with message : {}", message);
    try {
      ProductScoreUpdateDomainEventModel productScoreUpdateDomainEventModel =
        this.objectMapper.readValue(message, ProductScoreUpdateDomainEventModel.class);
      if (CollectionUtils.isNotEmpty(productScoreUpdateDomainEventModel.getProductCodes())) {
        for (String productCode : productScoreUpdateDomainEventModel.getProductCodes()) {
          List<ProductAndItemsVO> productAndItemsVOList =
              this.productService.updateProductScoreOnMasterDataChange(Constants.DEFAULT_STORE_ID, productCode, true,
                  null, false, false, null, null);
          for (ProductAndItemsVO productAndItemsVO : productAndItemsVOList)
            this.productAndItemSolrIndexerService.updateProductAndItemDetailsInSolr(
                productAndItemsVO.getProduct(), productAndItemsVO.getItems(), true);
        }
      }
    } catch (Exception e) {
      log.error("error while updating the product score for payload {}, error - ", message, e);
    }
  }
}
package com.gdn.partners.pbp.publisher.product;

import java.util.Map;
import java.util.Optional;

import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.service.ProductPublisherService;
import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.partners.pbp.publisher.Publisher;

@Component(value = ProductPublisherBean.BEAN_NAME + Publisher.SUFFIX_BEAN_NAME)
@Transactional(readOnly = true)
public class ProductPublisherBean implements Publisher {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductPublisherBean.class);

  public static final String BEAN_NAME = "product";

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProductCollectionRepository productCollectionRepository;

  @Autowired
  private ProductPublisherService productPublisherService;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void publish(Map<String, Object> datas) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    String appName = String.valueOf(datas.get(GdnBaseLookup.APP_NAME));
    String productCode = String.valueOf(datas.get(GdnBaseLookup.PRODUCT_CODE));
    try {
      if (GdnBaseLookup.APP_NAME_PDT.equals(appName)) {
        ProductCollection productCollection =
            this.productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
        boolean trustedSeller = Optional.ofNullable(
            businessPartnerRepository.filterDetailByBusinessPartnerCode(
              productCollection.getBusinessPartnerCode())).map(ProfileResponse::isTrustedSeller)
          .orElse(false);
        this.productPublisherService.publish(productCode, productCollection.getBusinessPartnerCode(),
            productCollection.getBusinessPartnerName(), GdnMandatoryRequestParameterUtil.getUsername(),
            productCollection.isPostLive(), productCollection.isRestrictedKeywordsPresent(),
            productCollection.getRestrictedKeywordsDetected(), productCollection.getPrioritySeller(),
          trustedSeller, productCollection.getProductId(), productCollection);
      }
    } catch (Exception e) {
      ProductPublisherBean.LOGGER.error("failed publish product", e);
    }
  }
}

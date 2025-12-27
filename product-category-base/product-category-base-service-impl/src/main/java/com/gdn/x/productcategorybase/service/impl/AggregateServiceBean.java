package com.gdn.x.productcategorybase.service.impl;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.messaging.MessageChannel;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.AggregateCommandDesc;
import com.gdn.x.productcategorybase.config.KafkaPublisher;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.brand.Brand;
import com.gdn.x.productcategorybase.properties.AggregatorProperties;
import com.gdn.x.productcategorybase.service.AggregateService;
import com.gdn.x.productcategorybase.service.AsyncProcessor;
import com.gdn.x.productcategorybase.service.ProductService;
import com.gdn.x.productcategorybase.service.ProductServiceWrapper;
import com.gdn.x.productcategorybase.service.brand.BrandService;
import com.gdn.x.productcategorybase.util.AggregateUtil;
import lombok.extern.slf4j.Slf4j;

@Lazy
@Slf4j
@Service
@Transactional(readOnly = true)
public class AggregateServiceBean implements AggregateService {

  private AggregatorProperties aggregatorProperties;

  private static final String HEADER_NAME = "kafka_topic";
  private static final String START = "Start";
  private static final String FINISH = "Finish";
  private static final String EMPTY_SPACE_SEPARATOR = " ";
  private static final String PAGE_COUNTER = "Page [{}/{} of {}]";
  private static final String DETAIL = "with totalPage {}/{}={}";
  private static final String BRAND = "Brand";
  private static final String BACKSLASH = "/";

  @Autowired
  private AsyncProcessor asyncProcessor;

  @Autowired
  private ProductService productService;

  @Autowired
  private BrandService brandServiceBean;

  @Autowired
  @Lazy
  private ProductServiceWrapper productServiceWrapper;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Autowired
  public AggregateServiceBean(AggregatorProperties aggregatorProperties,
      AsyncProcessor asyncProcessor, ProductService productService, ProductServiceWrapper productServiceWrapper, KafkaPublisher kafkaPublisher) {
    this.aggregatorProperties = aggregatorProperties;
    this.asyncProcessor = asyncProcessor;
    this.productService = productService;
    this.productServiceWrapper = productServiceWrapper;
    this.kafkaPublisher = kafkaPublisher;
  }

  private Optional<Product> getOptionalProduct(String storeId, String productCode) {
    return Optional.of(productCode)
        .map(pc -> {
          try {
            return productServiceWrapper.
                getCompleteProductDetailByProductCodeInAllProducts(storeId, pc);
          } catch (Exception e) {
            return null;
          }
        });
  }

  private void publishProduct(String storeId, String productCode) {
    getOptionalProduct(storeId, productCode).map(product -> {
      ProductDomainEventModel productDomainEventModel = AggregateUtil.toProductDomainEventModel(product);
      if (Objects.nonNull(productDomainEventModel)) {
        productDomainEventModel.setBrandLogoUrl(toBrandLogoUrl(getBrandCode(product.getProductAttributes())));
      }
      return productDomainEventModel;
    }).ifPresent(productDomainEventModel -> asyncProcessor.submitWithBackoff(AggregateCommandDesc.PRODUCT,
        () -> send(productDomainEventModel, DomainEventName.PRODUCT_PUBLISH_ALL)));
  }

  private void publishProductCategories(String storeId, String productCode) {
    getOptionalProduct(storeId,productCode)
        .map(Product::getProductCategories)
        .filter(pcs -> !CollectionUtils.isEmpty(pcs))
        .ifPresent(this::publishProductCategory);
  }

  private void publishProductCategory(List<ProductCategory> productCategories) {
    Optional.ofNullable(productCategories).map(AggregateUtil::toAggregateProductCategoryDomainEventModel).ifPresent(
        productCategoryDomainEventModel -> asyncProcessor.submitWithBackoff(AggregateCommandDesc.PRODUCT_CATEGORY,
            () -> send(productCategoryDomainEventModel, DomainEventName.PRODUCT_CATEGORY_PUBLISH_ALL)));
  }

  private void publishProductAttributes(String storeId, String productCode) {
    getOptionalProduct(storeId,productCode)
        .map(Product::getProductAttributes)
        .filter(pcs -> !CollectionUtils.isEmpty(pcs))
        .ifPresent(this::publishProductAttribute);
  }

  private void publishProductAttribute(List<ProductAttribute> productAttributes) {
    Optional.ofNullable(productAttributes).map(AggregateUtil::toAggregateProductAttributeDomainEventModel).ifPresent(
        productAttributeDomainEventModel -> asyncProcessor.submitWithBackoff(AggregateCommandDesc.PRODUCT_ATTRIBUTE,
            () -> send(productAttributeDomainEventModel, DomainEventName.PRODUCT_ATTRIBUTE_PUBLISH_ALL)));
  }

  private void publishImages(String storeId, String productCode) {
    getOptionalProduct(storeId,productCode)
        .map(Product::getProductImages)
        .filter(pcs -> !CollectionUtils.isEmpty(pcs))
        .ifPresent(this::publishImage);
  }

  private void publishImage(List<ProductImage> productImages) {
    Optional.ofNullable(productImages).map(AggregateUtil::toAggregateImageDomainEventModel).ifPresent(
        productImageEventModel -> asyncProcessor.submitWithBackoff(AggregateCommandDesc.IMAGE,
            () -> send(productImageEventModel, DomainEventName.IMAGE_PUBLISH_ALL)));
  }

  private void publishProductItems(String storeId, String productCode) {
    getOptionalProduct(storeId,productCode)
        .map(Product::getProductItems)
        .filter(pcs -> !CollectionUtils.isEmpty(pcs))
        .ifPresent(this::publishProductItem);
  }

  private void publishProductItem(List<ProductItem> productItems) {
    Optional.ofNullable(productItems).map(AggregateUtil::toAggregateProductItemDomainEventModel).ifPresent(
        productItemDomainEventModel -> asyncProcessor.submitWithBackoff(AggregateCommandDesc.PRODUCT_ITEM,
            () -> send(productItemDomainEventModel, DomainEventName.PRODUCT_ITEM_PUBLISH_ALL)));
  }

  private String toBrandLogoUrl(String brandCode) {
    return Optional.ofNullable(brandCode)
            .map(val -> {
              try {
                return brandServiceBean.findByBrandCode(val);
              } catch (Exception e) {
                log.error("Error occurred when findByBrandCode {}", val, e);
                return null;
              }
            })
            .map(Brand::getBrandLogoPath)
            .map(val -> BACKSLASH + brandCode + BACKSLASH + val)
            .orElse(null);
  }

  private String getBrandCode(List<ProductAttribute> productAttributes) {
    return Optional.ofNullable(productAttributes)
            .map(val -> val.stream()
                    .filter(productAttribute ->
                            Objects.nonNull(productAttribute.getProductAttributeName())
                                    && BRAND.equalsIgnoreCase(productAttribute.getProductAttributeName()))
                    .map(ProductAttribute::getProductAttributeValues)
                    .map(productAttributeValues -> productAttributeValues.stream()
                            .filter(Objects::nonNull)
                            .map(ProductAttributeValue::getPredefinedAllowedAttributeValue)
                            .map(PredefinedAllowedAttributeValue::getPredefinedAllowedAttributeCode)
                            .findFirst()
                            .orElse(null))
                    .findFirst()
                    .orElse(null)
            )
            .orElse(null);
  }

  @Override
  public void publishPageOfProducts(String storeId, int startPage) {
    Page<String> productCodePage;
    log.info(START + EMPTY_SPACE_SEPARATOR + AggregateCommandDesc.PRODUCT + EMPTY_SPACE_SEPARATOR + DETAIL,
        aggregatorProperties.getTotalData(), aggregatorProperties.getPageSize(), aggregatorProperties.getTotalPage());
    int ctrPage = 0;
    int totalPage = startPage + aggregatorProperties.getTotalPage();
    do {
      productCodePage = productService.getActiveProductCodes(storeId, PageRequest.of(startPage, aggregatorProperties.getPageSize()));
      log.info(AggregateCommandDesc.PRODUCT + EMPTY_SPACE_SEPARATOR + PAGE_COUNTER,
          startPage, totalPage, productCodePage.getTotalPages());
      productCodePage
          .getContent()
          .forEach(productCode -> publishProduct(storeId,productCode));
      startPage++;
      ctrPage++;
    } while (productCodePage.hasNext() && ctrPage<aggregatorProperties.getTotalPage());
    log.info(FINISH + EMPTY_SPACE_SEPARATOR + AggregateCommandDesc.PRODUCT);
  }

  @Override
  public void publishPageOfProductCategories(String storeId, int startPage){
    Page<String> productCodePage;
    log.info(START + EMPTY_SPACE_SEPARATOR + AggregateCommandDesc.PRODUCT_CATEGORY + EMPTY_SPACE_SEPARATOR + DETAIL,
        aggregatorProperties.getTotalData(), aggregatorProperties.getPageSize(), aggregatorProperties.getTotalPage());
    int ctrPage = 0;
    int totalPage = startPage + aggregatorProperties.getTotalPage();
    do {
      productCodePage = productService.getActiveProductCodes(storeId, PageRequest.of(startPage, aggregatorProperties.getPageSize()));
      log.info(AggregateCommandDesc.PRODUCT_CATEGORY + EMPTY_SPACE_SEPARATOR + PAGE_COUNTER,
          startPage, totalPage, productCodePage.getTotalPages());
      productCodePage
          .getContent()
          .forEach(productCode -> publishProductCategories(storeId,productCode));
      startPage++;
      ctrPage++;
    } while (productCodePage.hasNext() && ctrPage<aggregatorProperties.getTotalPage());
    log.info(FINISH + EMPTY_SPACE_SEPARATOR + AggregateCommandDesc.PRODUCT_CATEGORY);
  }

  @Override
  public void publishPageOfProductAttributes(String storeId, int startPage){
    Page<String> productCodePage;
    log.info(START + EMPTY_SPACE_SEPARATOR + AggregateCommandDesc.PRODUCT_ATTRIBUTE + EMPTY_SPACE_SEPARATOR + DETAIL,
        aggregatorProperties.getTotalData(), aggregatorProperties.getPageSize(), aggregatorProperties.getTotalPage());
    int ctrPage = 0;
    int totalPage = startPage + aggregatorProperties.getTotalPage();
    do {
      productCodePage = productService.getActiveProductCodes(storeId, PageRequest.of(startPage, aggregatorProperties.getPageSize()));
      log.info(AggregateCommandDesc.PRODUCT_ATTRIBUTE + EMPTY_SPACE_SEPARATOR + PAGE_COUNTER,
          startPage, totalPage, productCodePage.getTotalPages());
      productCodePage
          .getContent()
          .forEach(productCode -> publishProductAttributes(storeId,productCode));
      startPage++;
      ctrPage++;
    } while (productCodePage.hasNext() && ctrPage<aggregatorProperties.getTotalPage());
    log.info(FINISH + EMPTY_SPACE_SEPARATOR + AggregateCommandDesc.PRODUCT_ATTRIBUTE);
  }

  @Override
  public void publishPageOfImages(String storeId, int startPage){
    Page<String> productCodePage;
    log.info(START + EMPTY_SPACE_SEPARATOR + AggregateCommandDesc.IMAGE + EMPTY_SPACE_SEPARATOR + DETAIL,
        aggregatorProperties.getTotalData(), aggregatorProperties.getPageSize(), aggregatorProperties.getTotalPage());
    int ctrPage = 0;
    int totalPage = startPage + aggregatorProperties.getTotalPage();
    do {
      productCodePage = productService.getActiveProductCodes(storeId, PageRequest.of(startPage, aggregatorProperties.getPageSize()));
      log.info(AggregateCommandDesc.IMAGE + EMPTY_SPACE_SEPARATOR + PAGE_COUNTER,
          startPage, totalPage, productCodePage.getTotalPages());
      productCodePage
          .getContent()
          .forEach(productCode -> publishImages(storeId,productCode));
      startPage++;
      ctrPage++;
    } while (productCodePage.hasNext() && ctrPage<aggregatorProperties.getTotalPage());
    log.info(FINISH + EMPTY_SPACE_SEPARATOR + AggregateCommandDesc.IMAGE);
  }

  @Override
  public void publishPageOfProductItems(String storeId, int startPage){
    Page<String> productCodePage;
    log.info(START + EMPTY_SPACE_SEPARATOR + AggregateCommandDesc.PRODUCT_ITEM + EMPTY_SPACE_SEPARATOR + DETAIL,
        aggregatorProperties.getTotalData(), aggregatorProperties.getPageSize(), aggregatorProperties.getTotalPage());
    int ctrPage = 0;
    int totalPage = startPage + aggregatorProperties.getTotalPage();
    do {
      productCodePage = productService.getActiveProductCodes(storeId, PageRequest.of(startPage, aggregatorProperties.getPageSize()));
      log.info(AggregateCommandDesc.PRODUCT_ITEM + EMPTY_SPACE_SEPARATOR + PAGE_COUNTER,
          startPage, totalPage, productCodePage.getTotalPages());
      productCodePage
          .getContent()
          .forEach(productCode -> publishProductItems(storeId,productCode));
      startPage++;
      ctrPage++;
    } while (productCodePage.hasNext() && ctrPage<aggregatorProperties.getTotalPage());
    log.info(FINISH + EMPTY_SPACE_SEPARATOR + AggregateCommandDesc.PRODUCT_ITEM);
  }

  /**
   * method for send message to kafka
   * @param
   */
  public void send(Object object, String domainEventName) {
    this.kafkaPublisher.send(domainEventName, object);
  }

}

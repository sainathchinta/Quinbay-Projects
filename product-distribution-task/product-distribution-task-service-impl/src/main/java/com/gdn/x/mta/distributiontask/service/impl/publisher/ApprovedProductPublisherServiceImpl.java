package com.gdn.x.mta.distributiontask.service.impl.publisher;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.mta.product.enums.PrioritySeller;
import com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName;
import com.gdn.x.mta.distributiontask.domain.event.model.*;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductAttribute;
import com.gdn.x.mta.distributiontask.model.ProductImage;
import com.gdn.x.mta.distributiontask.model.ProductItem;
import com.gdn.x.mta.distributiontask.model.ProductItemAttribute;
import com.gdn.x.mta.distributiontask.model.ProductItemImage;
import com.gdn.x.mta.distributiontask.model.type.ReviewType;
import com.gdn.x.mta.distributiontask.service.api.publisher.ApprovedProductPublisherService;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaPublisher;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaTopicProperties;
import com.gdn.x.productcategorybase.domain.event.model.AllowedAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.CategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ImageDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.PredefinedAllowedAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductCategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductItemAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.dto.AttributeType;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Created by virajjasani on 02/10/16.
 */
@Service
@Slf4j
public class ApprovedProductPublisherServiceImpl implements ApprovedProductPublisherService {

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private ObjectMapper mapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Value("${replace.empty.review.type.vendor.edited}")
  private boolean replaceEmptyReviewTypeVendorEdited;

  @Override
  public PDTProductDomainEventModel publishUpdatedProductForFinalApproval(Product product, boolean reviewPending)
      throws Exception {
    GdnPreconditions.checkArgument(product != null,
        "Product not initialized. Cannot be sent to kafka");
    try {
      PDTProductDomainEventModel pdtProductDomainEventModel =
          convertProductToProductDomainEventModel(product, reviewPending);
      kafkaProducer.send(DomainEventName.PRODUCT_QC_APPROVED_TASK_EVENT_NAME, product.getProductCode(),
          pdtProductDomainEventModel);
      return pdtProductDomainEventModel;
    } catch (Exception e) {
      log.error(
          "error while sending product details to kafka for final approval. productCode: {}",
          product.getProductCode(), e);
      throw e;
    }
  }

  @Override
  public PDTProductVendorApprovedEventModel publishVendorApprovedEvent(Product product, boolean reviewPending) {
    GdnPreconditions.checkArgument(product != null, "Product not initialized. Cannot be sent to kafka");
    try {
      PDTProductVendorApprovedEventModel pdtProductVendorApprovedEventModel = new PDTProductVendorApprovedEventModel();
      pdtProductVendorApprovedEventModel.setProductCode(product.getProductCode());
      String vendorApprovedTaskEventName = kafkaTopicProperties.getProductVendorApprovedTaskEvent();
      if (product.getPrioritySeller() == PrioritySeller.PRIORITY_1.getPrioritySeller()) {
        vendorApprovedTaskEventName = kafkaTopicProperties.getProductVendorApprovedTaskPriority1Event();
      } else if (product.getPrioritySeller() == PrioritySeller.PRIORITY_2.getPrioritySeller()) {
        vendorApprovedTaskEventName = kafkaTopicProperties.getProductVendorApprovedTaskPriority2Event();
      }
      log.info("vendorApprovedTaskEventName: {} and pdtProductVendorApprovedEventModel : {}",
          vendorApprovedTaskEventName, pdtProductVendorApprovedEventModel);
      kafkaProducer.send(vendorApprovedTaskEventName, product.getProductCode(), pdtProductVendorApprovedEventModel);
      return pdtProductVendorApprovedEventModel;
    } catch (Exception e) {
      log.error("error while sending product details to kafka for final approval. productCode: {} ",
          product.getProductCode(), e);
      throw e;
    }
  }

  @Override
  public PDTEditedProductVendorApprovedEventModel publishEditedVendorApprovedEvent(Product product) {
    GdnPreconditions.checkArgument(Objects.nonNull(product), "Product not initialized. Cannot be sent to kafka");
    try {
      PDTEditedProductVendorApprovedEventModel pdtEditedProductVendorApprovedEventModel =
          new PDTEditedProductVendorApprovedEventModel();
      pdtEditedProductVendorApprovedEventModel.setProductCode(product.getProductCode());
      pdtEditedProductVendorApprovedEventModel.setApprovalType(handleNullReviewType(product));
      pdtEditedProductVendorApprovedEventModel.setUpdatedBy(product.getUpdatedBy());
      kafkaProducer.send(kafkaTopicProperties.getEditedProductVendorApprovedTaskEvent(), product.getProductCode(),
          pdtEditedProductVendorApprovedEventModel);
      return pdtEditedProductVendorApprovedEventModel;
    } catch (Exception e) {
      log.error("error while sending edited product details to kafka for final approval. productCode: {} ",
          product.getProductCode(), e);
      throw e;
    }
  }

  @Override
  public PDTRevisedProductVendorApprovedEventModel publishRevisedVendorApprovedEvent(Product product,
      boolean reviewPending) {
    GdnPreconditions.checkArgument(Objects.nonNull(product), "Product not initialized. Cannot be sent to kafka");
    try {
      PDTRevisedProductVendorApprovedEventModel pdtRevisedProductVendorApprovedEventModel =
          new PDTRevisedProductVendorApprovedEventModel();
      pdtRevisedProductVendorApprovedEventModel.setProductCode(product.getProductCode());
      if (Objects.nonNull(product.getReviewType()) && product.isEdited() && !product.isRevised()) {
        pdtRevisedProductVendorApprovedEventModel.setApprovalType(product.getReviewType().name());
      }
      pdtRevisedProductVendorApprovedEventModel.setPostLive(product.isPostLive());
      pdtRevisedProductVendorApprovedEventModel.setReviewPending(reviewPending);
      pdtRevisedProductVendorApprovedEventModel.setUpdatedBy(product.getUpdatedBy());
      log.info("Publishing event {} for product code : {} and review pending : {}",
          kafkaTopicProperties.getRevisedProductVendorApprovedTaskEvent(), product.getProductCode(), reviewPending);
      kafkaProducer.send(kafkaTopicProperties.getRevisedProductVendorApprovedTaskEvent(), product.getProductCode(),
          pdtRevisedProductVendorApprovedEventModel);
      return pdtRevisedProductVendorApprovedEventModel;
    } catch (Exception e) {
      log.error("error while sending product details to kafka for final approval. productCode: {} ",
          product.getProductCode(), e);
      throw e;
    }
  }

  private String handleNullReviewType(Product product) {
    if(replaceEmptyReviewTypeVendorEdited && Objects.isNull(product.getReviewType())){
      log.info("review type was override for product : {} ", product.getProductCode());
      return ReviewType.CONTENT_AND_IMAGE.name();
    }
     return product.getReviewType().name();
  }


  @Override
  public PDTProductDomainEventModel convertProductToProductDomainEventModel(Product product, boolean reviewPending)
      throws Exception {
    PDTProductDomainEventModel pdtProductDomainEventModel = new PDTProductDomainEventModel();
    BeanUtils.copyProperties(product, pdtProductDomainEventModel, "productImages", "productItems",
        "productAttributes");
    pdtProductDomainEventModel.setName(product.getProductName());
    pdtProductDomainEventModel.setActivated(true);
    pdtProductDomainEventModel.setViewable(false);
    pdtProductDomainEventModel.setUrl(product.getVideoUrl());
    pdtProductDomainEventModel.setMerchantCode(product.getBusinessPartnerCode());
    pdtProductDomainEventModel.setMerchantName(product.getBusinessPartnerName());
    pdtProductDomainEventModel.setReviewPending(reviewPending);
    pdtProductDomainEventModel.setMarginExceeded(product.isMarginExceeded());
    pdtProductDomainEventModel.setBrandApprovalStatus(product.getBrandApprovalStatus());
    pdtProductDomainEventModel.setState(product.getState());
    if (StringUtils.isNotEmpty(product.getProductNotes())) {
      PDTProductNotesDomainEventModel pdtProductNotesDomainEventModel =
          mapper.readValue(product.getProductNotes(), PDTProductNotesDomainEventModel.class);
      pdtProductDomainEventModel.setProductNotes(pdtProductNotesDomainEventModel);
    }
    setImageDomainEventModel(product, pdtProductDomainEventModel, reviewPending);
    LinkedHashMap<String, ProductAttribute> productAttributeMap = product.getProductAttributes().stream()
      .collect(Collectors.toMap(ProductAttribute::getAttributeCode, Function.identity(),
        (attributeCode1, attributeCode2) -> attributeCode1, LinkedHashMap::new));
    List<ProductAttribute> productAttributes =
      productAttributeMap.values().stream().distinct().collect(Collectors.toList());
    product.setProductAttributes(productAttributes);
    setProductAttributeDomainEventModel(product, pdtProductDomainEventModel);
    setProductItemDomainEventModel(product, pdtProductDomainEventModel, reviewPending);
    setProductCategoryCodeAndName(product, pdtProductDomainEventModel);
    log.info("Product-workflow-tracker : qc approved event published PDT for productCode : {}",
        product.getProductCode());
    return pdtProductDomainEventModel;
  }

  @Override
  public PDTAutoApprovalEventModel publishAutoApprovalEvent(PDTAutoApprovalEventModel pdtAutoApprovalEventModel) {
    log.info("Publishing event for auto approval of new product {}, pdtAutoApprovalEventModel : {} ",
        DomainEventName.PDT_PRODUCT_AUTO_APPROVAL_EVENT, pdtAutoApprovalEventModel);
    kafkaProducer.send(DomainEventName.PDT_PRODUCT_AUTO_APPROVAL_EVENT, pdtAutoApprovalEventModel.getProductCode(),
        pdtAutoApprovalEventModel);
    return pdtAutoApprovalEventModel;
  }

  private void setProductCategoryCodeAndName(Product product, PDTProductDomainEventModel productDomainEventModel) {
    ProductCategoryDomainEventModel productCategoryDomainEventModel = new ProductCategoryDomainEventModel();
    CategoryDomainEventModel categoryDomainEventModel = new CategoryDomainEventModel();
    categoryDomainEventModel.setCategoryCode(product.getCategoryCode());
    categoryDomainEventModel.setName(product.getCategoryName());
    productCategoryDomainEventModel.setCategory(categoryDomainEventModel);
    productDomainEventModel.setProductCategories(Arrays.asList(productCategoryDomainEventModel));
  }

  private void setProductItemDomainEventModel(Product product,
      PDTProductDomainEventModel pdtProductDomainEventModel, boolean reviewPending) throws IOException {
    List<PDTProductItemDomainEventModel> productItemDomainEventModelList = new ArrayList<>();
    for (ProductItem productItem : product.getProductItems()) {
      PDTProductItemDomainEventModel pdtProductItemDomainEventModel = new PDTProductItemDomainEventModel();
      BeanUtils
          .copyProperties(productItem, pdtProductItemDomainEventModel, "product", "productItemImages",
              "productItemAttributes", "dangerousGoodsLevel");
      pdtProductItemDomainEventModel.setActivated(true);
      pdtProductItemDomainEventModel.setViewable(false);
      pdtProductItemDomainEventModel.setDangerousGoodsLevel(Objects.nonNull(productItem.getDangerousGoodsLevel()) ?
          productItem.getDangerousGoodsLevel() :
          Integer.valueOf(0));
      List<ProductItemImage> productItemImages = productItem.getProductItemImages();
      if (reviewPending) {
        productItemImages = productItemImages.stream()
            .filter(productItemImage -> !Optional.ofNullable(productItemImage.getOriginalImage()).orElse(false))
            .collect(Collectors.toList());
      }
      List<ImageDomainEventModel> itemImageDomainEventModelList = new ArrayList<>();
      List<PDTImageDomainEventModel> pdtItemImageDomainEventModelList = new ArrayList<>();
      for (ProductItemImage productItemImage : productItemImages) {
        ImageDomainEventModel imageDomainEventModel = new ImageDomainEventModel();
        if (productItemImage.getMainImage() != null) {
          imageDomainEventModel.setMainImage(productItemImage.getMainImage());
        }
        imageDomainEventModel.setLocationPath(productItemImage.getLocationPath());
        imageDomainEventModel.setSequence(productItemImage.getSequence());
        imageDomainEventModel.setCommonImage(productItemImage.isCommonImage());

        itemImageDomainEventModelList.add(imageDomainEventModel);
        PDTImageDomainEventModel pdtImageDomainEventModel = new PDTImageDomainEventModel();
        BeanUtils.copyProperties(productItemImage, pdtImageDomainEventModel, "productItem");
        pdtImageDomainEventModel.setMainImage(productItemImage.getMainImage());
        pdtItemImageDomainEventModelList.add(pdtImageDomainEventModel);
      }
      pdtProductItemDomainEventModel.setImages(itemImageDomainEventModelList);
      pdtProductItemDomainEventModel.setPdtImageDomainEventModels(pdtItemImageDomainEventModelList);
      List<ProductItemAttribute> productItemAttributes =
        productItem.getProductItemAttributes().stream().distinct().collect(
            Collectors.toMap(ProductItemAttribute::getAttributeCode, Function.identity(),
              (attributeCode1, attributeCode2) -> attributeCode1, LinkedHashMap::new)).values()
          .stream().distinct().collect(Collectors.toList());
      productItem.setProductItemAttributes(productItemAttributes);
      setProductItemAttributeDomainEventModel(productItem, pdtProductItemDomainEventModel);
      if (StringUtils.isNotEmpty(productItem.getItemNotes())) {
        PDTItemNotesDomainEventModel pdtItemNotesDomainEventModel =
            mapper.readValue(productItem.getItemNotes(), PDTItemNotesDomainEventModel.class);
        pdtProductItemDomainEventModel.setItemNotes(pdtItemNotesDomainEventModel);
      }
      productItemDomainEventModelList.add(pdtProductItemDomainEventModel);
    }
    pdtProductDomainEventModel.setProductItems(productItemDomainEventModelList);
  }

  private void setProductItemAttributeDomainEventModel(ProductItem productItem,
      PDTProductItemDomainEventModel productItemDomainEventModel) {
    List<ProductItemAttributeValueDomainEventModel> productItemAttributeValueDomainEventModels = new ArrayList<>();
    Map<String, String> itemAttributeMap = new HashMap<>();

    for (ProductItemAttribute productItemAttribute : productItem.getProductItemAttributes()) {
      if (!productItemAttribute.isMarkForDelete() && !itemAttributeMap
          .containsKey(productItemAttribute.getAttributeCode())) {
        itemAttributeMap.putIfAbsent(productItemAttribute.getAttributeCode(), productItemAttribute.getAttributeCode());
        ProductItemAttributeValueDomainEventModel model = new ProductItemAttributeValueDomainEventModel();
        AttributeDomainEventModel attributeDomainEventModel = new AttributeDomainEventModel();
        attributeDomainEventModel.setAttributeCode(productItemAttribute.getAttributeCode());
        attributeDomainEventModel.setAttributeType(productItemAttribute.getAttributeType());
        attributeDomainEventModel.setName(productItemAttribute.getName());
        model.setAttribute(attributeDomainEventModel);
        model.setValue(productItemAttribute.getValue());
        productItemAttributeValueDomainEventModels.add(model);
      }
    }
    productItemDomainEventModel.setProductItemAttributeValues(productItemAttributeValueDomainEventModels);
  }

  private void setProductAttributeDomainEventModel(Product product,
      PDTProductDomainEventModel pdtProductDomainEventModel) {
    List<ProductAttributeDomainEventModel> productAttributeDomainEventModelList = new ArrayList<>();
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      if (!productAttribute.isMarkForDelete()) {
        ProductAttributeDomainEventModel productAttributeDomainEventModel = new ProductAttributeDomainEventModel();
        productAttributeDomainEventModel.setProductAttributeName(productAttribute.getName());
        AttributeDomainEventModel attributeDomainEventModel = new AttributeDomainEventModel();
        attributeDomainEventModel.setName(productAttribute.getName());
        attributeDomainEventModel.setAttributeType(productAttribute.getAttributeType());
        attributeDomainEventModel.setAttributeCode(productAttribute.getAttributeCode());
        productAttributeDomainEventModel.setAttribute(attributeDomainEventModel);
        setProductAttributeValueDomainEventModel(productAttribute, productAttributeDomainEventModel);
        productAttributeDomainEventModelList.add(productAttributeDomainEventModel);
      }
    }
    pdtProductDomainEventModel.setProductAttributes(productAttributeDomainEventModelList);
  }

  private void setProductAttributeValueDomainEventModel(ProductAttribute productAttribute,
      ProductAttributeDomainEventModel productAttributeDomainEventModel) {
    List<ProductAttributeValueDomainEventModel> productAttributeValueDomainEventModelList =
        new ArrayList<>();
    ProductAttributeValueDomainEventModel productAttributeValueDomainEventModel =
        new ProductAttributeValueDomainEventModel();
    if (productAttribute.getAttributeType().equals(AttributeType.DEFINING_ATTRIBUTE.toString())) {
      AllowedAttributeValueDomainEventModel allowedAttributeValueDomainEventModel =
          new AllowedAttributeValueDomainEventModel();
      allowedAttributeValueDomainEventModel.setValue(productAttribute.getValue());
      allowedAttributeValueDomainEventModel
          .setAllowedAttributeCode(productAttribute.getAttributeCode());
      productAttributeValueDomainEventModel
          .setAllowedAttributeValue(allowedAttributeValueDomainEventModel);
    } else if (productAttribute.getAttributeType()
        .equals(AttributeType.PREDEFINED_ATTRIBUTE.toString())) {
      PredefinedAllowedAttributeValueDomainEventModel
          predefinedAllowedAttributeValueDomainEventModel =
          new PredefinedAllowedAttributeValueDomainEventModel();
      predefinedAllowedAttributeValueDomainEventModel
          .setPredefinedAllowedAttributeCode(productAttribute.getAttributeCode());
      predefinedAllowedAttributeValueDomainEventModel.setValue(productAttribute.getValue());
      productAttributeValueDomainEventModel
          .setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueDomainEventModel);
    } else {
      productAttributeValueDomainEventModel
          .setDescriptiveAttributeValue(productAttribute.getValue());
    }
    productAttributeValueDomainEventModelList.add(productAttributeValueDomainEventModel);
    productAttributeDomainEventModel
        .setProductAttributeValues(productAttributeValueDomainEventModelList);
  }

  private void setImageDomainEventModel(Product product,
      PDTProductDomainEventModel pdtProductDomainEventModel, boolean reviewPending) {
    List<ImageDomainEventModel> imageDomainEventModelList = new ArrayList<>();
    List<PDTImageDomainEventModel> pdtImageDomainEventModels = new ArrayList<>();
    List<ProductImage> productImages = product.getProductImages();
    if (reviewPending) {
      productImages = productImages.stream()
          .filter(productImage -> !Optional.ofNullable(productImage.getOriginalImage()).orElse(false))
          .collect(Collectors.toList());
    }
    for (ProductImage productImage : productImages) {
      ImageDomainEventModel imageDomainEventModel = new ImageDomainEventModel();
      BeanUtils.copyProperties(productImage, imageDomainEventModel, "product");
      imageDomainEventModel.setMainImage(productImage.isMainImage());
      imageDomainEventModelList.add(imageDomainEventModel);
      PDTImageDomainEventModel pdtImageDomainEventModel = new PDTImageDomainEventModel();
      BeanUtils.copyProperties(productImage, pdtImageDomainEventModel, "product");
      pdtImageDomainEventModel.setMainImage(productImage.isMainImage());
      pdtImageDomainEventModels.add(pdtImageDomainEventModel);
    }
    pdtProductDomainEventModel.setPdtImageDomainEventModels(pdtImageDomainEventModels);
    pdtProductDomainEventModel.setImages(imageDomainEventModelList);
  }

}

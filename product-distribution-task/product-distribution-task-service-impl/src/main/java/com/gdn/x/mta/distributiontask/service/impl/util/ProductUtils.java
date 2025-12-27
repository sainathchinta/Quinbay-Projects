package com.gdn.x.mta.distributiontask.service.impl.util;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.hibernate.Hibernate;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.modal.PDTDimensionRefreshEventModel;
import com.gdn.x.mta.distributiontask.dao.api.ProductServiceRepository;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductAttribute;
import com.gdn.x.mta.distributiontask.model.ProductImage;
import com.gdn.x.mta.distributiontask.model.ProductItem;
import com.gdn.x.mta.distributiontask.model.ProductItemAttribute;
import com.gdn.x.mta.distributiontask.model.ProductItemImage;
import com.gdn.x.mta.distributiontask.model.dto.ProductHistoryDTO;
import com.gdn.x.mta.distributiontask.model.dto.ProductItemImageChangeHistoryDto;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductNotesRequest;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.google.api.services.youtube.YouTube;
import com.google.api.services.youtube.model.VideoListResponse;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Component
public class ProductUtils {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductServiceRepository productServiceRepository;

  @Value(value = "${youtube.data.api.key}")
  private String youTubeDataApiKey;

  @Value("${add.delete.variants.switch}")
  private boolean addDeleteVariantSwitch;

  @Value(value = "${vendor.search.auto.heal.regex}")
  private String vendorSearchAutoHealRegex;

  private static final String ID = "id";
  private static final String PRODUCT_IMAGES = "productImages";
  private static final String PRODUCT_ITEMS = "productItems";
  private static final String PRODUCT_ATTRIBUTES = "productAttributes";
  private static final String CURRENT_VENDOR = "currentVendor";
  private static final String REJECTED_COUNT = "rejectedCount";
  private static final String CREATED_DATE = "createdDate";
  private static final String CREATED_BY = "createdBy";
  private static final String VERSION = "version";
  private static final String PRODUCT_ITEM_IMAGES = "productItemImages";
  private static final String ITEM_NOTES = "itemNotes";
  private static final String PRODUCT_ITEM_ATTRIBUTES = "productItemAttributes";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String BUSINESS_PARTNER_NAME = "businessPartnerName";
  private static final String PRODUCT_CREATED_DATE = "productCreatedDate";
  private static final String MIGRATED_PRODUCT_CREATED_BY = "migratedProductCreatedBy";
  private static final String MIGRATED_PRODUCT_STATE = "migratedProductState";
  private static final String STATE = "state";
  private static final String STORE_ID = "storeId";
  private static final String IMAGE_DIFFICULTY_LEVEL = "imageDifficultyLevel";
  private static final String RESTRICTED_KEYWORD_PRESENT = "restrictedKeywordsPresent";
  private static final String IMAGE_VIOLATIONS = "imageViolations";
  private static final String PRODUCT_PREDICTION_SCORE = "productPredictionScore";
  private static final String VIDEO_LIST = "snippet,contentDetails,statistics";
  private static final String YOUTUBE_REGEX = "^(http(s)?:\\/\\/)?((w){3}.)?youtu(be|.be)?(\\.com)?\\/.+";
  private static final Pattern YOUTUBE_PATTERN = Pattern.compile(YOUTUBE_REGEX);
  private static final String VIDEO_REGEX =
      "(?<=watch\\?v=|/videos/|embed\\/|youtu.be\\/|\\/v\\/|\\/e\\/|watch\\?v%3D|watch\\?feature=player_embedded&v=|%2Fvideos%2F|embed%\u200C\u200B2F|youtu.be%2F|%2Fv%2F)[^#\\&\\?\\n]*";
  private static final Pattern VIDEO_PATTERN = Pattern.compile(VIDEO_REGEX);
  private static final String EDITED = "edited";
  private static final String REVISED = "revised";
  private static final String PRODUCT_NOTES = "productNotes";
  private static final String PRIORITY_SELLER = "prioritySeller";
  private static final String REVIEW_TPYE = "reviewType";
  private static final String COMMA = ",";

  public Product regenerateProductReplacementDetails(Product oldProduct, Product newProduct) {
    BeanUtils.copyProperties(newProduct, oldProduct, ID, STORE_ID, PRODUCT_IMAGES, PRODUCT_ITEMS,
        PRODUCT_ATTRIBUTES, REJECTED_COUNT, CREATED_DATE, CREATED_BY, PRODUCT_CREATED_DATE,
        MIGRATED_PRODUCT_CREATED_BY, MIGRATED_PRODUCT_STATE, "state", "currentVendor", "reviewType");
    for (ProductImage productImage : newProduct.getProductImages()) {
      productImage.setProduct(oldProduct);
    }
    for (ProductAttribute productAttribute : newProduct.getProductAttributes()) {
      productAttribute.setProduct(oldProduct);
    }
    for (ProductItem productItem : newProduct.getProductItems()) {
      productItem.setProduct(oldProduct);
    }
    oldProduct.setForceReview(newProduct.isForceReview());
    oldProduct.getProductItems().addAll(newProduct.getProductItems());
    oldProduct.getProductImages().addAll(newProduct.getProductImages());
    oldProduct.getProductAttributes().addAll(newProduct.getProductAttributes());
    oldProduct.setDistributionMappingStatus(newProduct.getDistributionMappingStatus());
    oldProduct.setProductCreationType(newProduct.getProductCreationType());
    return oldProduct;
  }

  public void regenerateProductImageDetails(Product product) {
    for (ProductItem productItem : product.getProductItems()) {
      if (CollectionUtils.isEmpty(productItem.getProductItemImages())) {
        productItem.setProductItemImages(new ArrayList<>());
        for (ProductImage productImage : product.getProductImages()) {
          ProductItemImage productItemImage = new ProductItemImage();
          productItemImage.setStoreId(productImage.getStoreId());
          productItemImage.setProductItem(productItem);
          productItemImage.setLocationPath(productImage.getLocationPath());
          productItemImage.setMainImage(productImage.isMainImage());
          productItemImage.setEdited(productImage.isEdited());
          productItemImage.setActive(productImage.isActive());
          productItemImage.setSequence(productImage.getSequence());
          productItemImage.setActive(productImage.isActive());
          productItemImage.setEdited(productImage.isEdited());
          productItemImage.setRevised(productImage.isRevised());
          productItem.getProductItemImages().add(productItemImage);
        }
      }
    }
  }

  public Product replaceProductImageDetails(Product existingProduct, Product newProduct) throws IOException {
    existingProduct.setImageDifficultyLevel(newProduct.getImageDifficultyLevel());
    existingProduct.getProductImages().clear();
    if (StringUtils.isNotEmpty(newProduct.getProductNotes())) {
      existingProduct.setProductNotes(StringUtils.isEmpty(existingProduct.getProductNotes()) ?
          newProduct.getProductNotes() :
          objectMapper.writeValueAsString(getProductNotesRequest(existingProduct, newProduct)));
    }
    List<ProductImage> productImageList = new ArrayList<>();
    for (ProductImage productImage : newProduct.getProductImages()) {
      productImage.setProduct(existingProduct);
      productImageList.add(productImage);
    }
    existingProduct.getProductImages().addAll(productImageList);
    for (ProductItem newProductItem : newProduct.getProductItems()) {
      for (ProductItem existingProductItem : existingProduct.getProductItems()) {
        if (existingProductItem.getSkuCode().equals(newProductItem.getSkuCode())) {
          existingProductItem.getProductItemImages().clear();
          List<ProductItemImage> productItemImageList = new ArrayList<>();
          for (ProductItemImage productItemImage : newProductItem.getProductItemImages()) {
            productItemImage.setProductItem(existingProductItem);
            productItemImageList.add(productItemImage);
          }
          if (StringUtils.isNotEmpty(newProductItem.getItemNotes())) {
            existingProductItem.setItemNotes(newProductItem.getItemNotes());
          }
          existingProductItem.getProductItemImages().addAll(productItemImageList);
          break;
        }
      }
    }
    return existingProduct;
  }

  private ProductNotesRequest getProductNotesRequest(Product existingProduct, Product newProduct) throws IOException {
    log.info("getProductNotesRequest existingProduct : {}, newProduct : {}", existingProduct, newProduct);
    ProductNotesRequest existingProductNotesRequest =
        objectMapper.readValue(existingProduct.getProductNotes(), ProductNotesRequest.class);
    ProductNotesRequest newProductNotesRequest =
        objectMapper.readValue(newProduct.getProductNotes(), ProductNotesRequest.class);
    existingProductNotesRequest.setAllVariants(newProductNotesRequest.getAllVariants());
    existingProductNotesRequest.setImageReason(newProductNotesRequest.getImageReason());
    existingProductNotesRequest.setCommonImageReason(newProductNotesRequest.getCommonImageReason());
    existingProductNotesRequest.setImagesAdditionalNotes(newProductNotesRequest.getImagesAdditionalNotes());
    log.info("replaceProductImageDetails existingProductNotesRequest : {}, newProductNotesRequest : {}",
        existingProductNotesRequest, newProductNotesRequest);
    return existingProductNotesRequest;
  }

  public Product replaceProductImageAndProductItemImages(Product existingProduct, Product newProduct) {
    existingProduct.setImageDifficultyLevel(newProduct.getImageDifficultyLevel());
    existingProduct.getProductImages().clear();
    List<ProductImage> productImageList = new ArrayList<>();
    for (ProductImage productImage : newProduct.getProductImages()) {
      productImage.setProduct(existingProduct);
      productImageList.add(productImage);
    }
    existingProduct.getProductImages().addAll(productImageList);
    for (ProductItem newProductItem : newProduct.getProductItems()) {
      for (ProductItem existingProductItem : existingProduct.getProductItems()) {
        if (existingProductItem.getSkuCode().equals(newProductItem.getSkuCode())) {
          existingProductItem.setUpcCode(newProductItem.getUpcCode());
          existingProductItem.getProductItemImages().clear();
          List<ProductItemImage> productItemImageList = new ArrayList<>();
          for (ProductItemImage productItemImage : newProductItem.getProductItemImages()) {
            productItemImage.setProductItem(existingProductItem);
            productItemImageList.add(productItemImage);
          }
          existingProductItem.getProductItemImages().addAll(productItemImageList);
          break;
        }
      }
    }
    return existingProduct;
  }

  public Product replaceProductDetails(Product existingProduct, Product newProduct, boolean retainNotes) {
    BeanUtils.copyProperties(newProduct, existingProduct, ID, PRODUCT_IMAGES, PRODUCT_ITEMS,
        PRODUCT_ATTRIBUTES, REJECTED_COUNT, CREATED_DATE, CREATED_BY, CURRENT_VENDOR, VERSION,
        BUSINESS_PARTNER_NAME, BUSINESS_PARTNER_CODE, PRODUCT_CREATED_DATE,
        MIGRATED_PRODUCT_CREATED_BY, MIGRATED_PRODUCT_STATE, STATE, STORE_ID,
        IMAGE_DIFFICULTY_LEVEL, RESTRICTED_KEYWORD_PRESENT,
        IMAGE_VIOLATIONS, PRODUCT_PREDICTION_SCORE, EDITED, REVIEW_TPYE, REVISED, PRODUCT_NOTES, PRIORITY_SELLER);
    if (!retainNotes) {
      existingProduct.setProductNotes(null);
    }
    replaceProductAttributes(existingProduct, newProduct);
    replaceProductItems(existingProduct, newProduct, retainNotes);
    existingProduct.setForceReview(newProduct.isForceReview());
    existingProduct.setDistributionMappingStatus(newProduct.getDistributionMappingStatus());
    existingProduct.setProductCreationType(newProduct.getProductCreationType());
    return existingProduct;
  }

  private void replaceProductItems(Product existingProduct, Product newProduct, boolean retainNotes) {
    List<ProductItem> productItemList = new ArrayList<>();
    for (ProductItem productItem : newProduct.getProductItems()) {
      ProductItem existingProductItem = existingProduct.getProductItems().stream()
          .filter(item -> Objects.nonNull(item.getSkuCode()))
          .filter(item -> item.getSkuCode().equals(productItem.getSkuCode())).findFirst().orElse(null);
      if (Objects.isNull(existingProductItem) && addDeleteVariantSwitch) {
        existingProductItem = new ProductItem();
        List<ProductItemImage> productItemImageList = new ArrayList<>();
        for (ProductItemImage productItemImage : productItem.getProductItemImages()) {
          productItemImage.setProductItem(existingProductItem);
          productItemImageList.add(productItemImage);
        }
        existingProductItem.getProductItemImages().addAll(productItemImageList);
      }
      if(Objects.nonNull(existingProductItem)) {
        BeanUtils.copyProperties(productItem, existingProductItem, ID, PRODUCT_ITEM_IMAGES,
            PRODUCT_ITEM_ATTRIBUTES, VERSION, CREATED_DATE, CREATED_BY, STORE_ID, ITEM_NOTES);
        replaceProductItemAttributes(productItem, existingProductItem);
        existingProductItem.setProduct(existingProduct);
        existingProductItem.setMarkForDelete(false);
        if (!retainNotes) {
          existingProductItem.setItemNotes(null);
        }
        productItemList.add(existingProductItem);
      }
    }
    existingProduct.getProductItems().clear();
    existingProduct.getProductItems().addAll(productItemList);
  }

  private void replaceProductItemAttributes(ProductItem productItem, ProductItem existingProductItem) {
    Map<String, ProductItemAttribute> existingProductItemAttributeMap =
        existingProductItem.getProductItemAttributes().stream()
            .filter(productItemAttribute -> !productItemAttribute.isMarkForDelete()).collect(Collectors
            .toMap(ProductItemAttribute::getAttributeCode, Function.identity(),
                (productAttribute1, productAttribute2) -> productAttribute1));
    Map<String, ProductItemAttribute> newProductItemAttributeMap = productItem.getProductItemAttributes().stream()
        .filter(productItemAttribute -> !productItemAttribute.isMarkForDelete()).collect(Collectors
            .toMap(ProductItemAttribute::getAttributeCode, Function.identity(),
                (productAttribute1, productAttribute2) -> productAttribute1));
    List<ProductItemAttribute> tempProductItemAttributes = new ArrayList<>();
    for (ProductItemAttribute newProductItemAttribute : productItem.getProductItemAttributes()) {
      if (!existingProductItemAttributeMap.containsKey(newProductItemAttribute.getAttributeCode())
          && !newProductItemAttribute.isMarkForDelete()) {
        ProductItemAttribute productItemAttribute = new ProductItemAttribute();
        productItemAttribute.setProduct(existingProductItem);
        productItemAttribute
            .setAttributeCode(newProductItemAttribute.getAttributeCode());
        productItemAttribute
            .setAttributeType(newProductItemAttribute.getAttributeType());
        productItemAttribute.setName(newProductItemAttribute.getName());
        productItemAttribute.setValue(newProductItemAttribute.getValue());
        productItemAttribute.setVariantCreation(newProductItemAttribute.isVariantCreation());
        tempProductItemAttributes.add(productItemAttribute);
      }
    }
    for (ProductItemAttribute oldProductItemAttribute : existingProductItem.getProductItemAttributes()) {
      if (newProductItemAttributeMap.containsKey(oldProductItemAttribute.getAttributeCode()) && !oldProductItemAttribute
          .isMarkForDelete()) {
        oldProductItemAttribute
            .setValue(newProductItemAttributeMap.get(oldProductItemAttribute.getAttributeCode()).getValue());
      } else {
        oldProductItemAttribute.setMarkForDelete(true);
      }
    }

    if (!CollectionUtils.isEmpty(tempProductItemAttributes)) {
      existingProductItem.getProductItemAttributes().addAll(tempProductItemAttributes);
    }
  }

  private void replaceProductAttributes(Product existingProduct, Product newProduct) {
    List<ProductAttribute> tempProductAttributes = new ArrayList<>();
    Map<String, ProductAttribute> existingProductAttributesMapExcludingDefiningAttributes =
        existingProduct.getProductAttributes().stream().filter(productAttribute -> !productAttribute.isMarkForDelete())
            .filter(productAttribute -> !isDefiningOrVariantCreationTrueBasedOnAddDeleteVariantSwitch(productAttribute)).collect(Collectors
            .toMap(productAttribute -> getKeyBasedOnAttributeType(productAttribute), Function.identity(),
                (productAttribute1, productAttribute2) -> productAttribute1));

    for (ProductAttribute newProductAttribute : newProduct.getProductAttributes()) {
      if (!isDefiningOrVariantCreationTrueBasedOnAddDeleteVariantSwitch(newProductAttribute)
          && !newProductAttribute.isMarkForDelete()) {
        boolean newAttribute;
        newAttribute =
            existingProductAttributesMapExcludingDefiningAttributes.containsKey(getKeyBasedOnAttributeType(newProductAttribute));
        if (!newAttribute) {
          newProductAttribute.setProduct(existingProduct);
          tempProductAttributes.add(newProductAttribute);
        }
      }
    }

    Map<String, ProductAttribute> newProductAttributesMap =
        newProduct.getProductAttributes().stream().filter(productAttribute -> !productAttribute.isMarkForDelete())
            .filter(productAttribute -> !isDefiningOrVariantCreationTrueBasedOnAddDeleteVariantSwitch(productAttribute)).collect(Collectors
            .toMap(productAttribute -> getKeyBasedOnAttributeType(productAttribute), Function.identity(),
                (productAttribute1, productAttribute2) -> productAttribute1));

    for (ProductAttribute oldProductAttribute : existingProduct.getProductAttributes()) {
      if (!isDefiningOrVariantCreationTrueBasedOnAddDeleteVariantSwitch(oldProductAttribute)
          && !oldProductAttribute.isMarkForDelete()) {
        if (newProductAttributesMap.containsKey(getKeyBasedOnAttributeType(oldProductAttribute))) {
          oldProductAttribute.setValue(newProductAttributesMap.get(getKeyBasedOnAttributeType(oldProductAttribute)).getValue());
          oldProductAttribute.setExtractedValue(newProductAttributesMap.get(getKeyBasedOnAttributeType(oldProductAttribute)).isExtractedValue());
          oldProductAttribute.setDsExtraction(newProductAttributesMap.get(getKeyBasedOnAttributeType(oldProductAttribute)).isDsExtraction());
        } else {
          oldProductAttribute.setMarkForDelete(true);
        }
      }
    }
    if (!CollectionUtils.isEmpty(tempProductAttributes)) {
      existingProduct.getProductAttributes().addAll(tempProductAttributes);
    }
  }

  private boolean isDefiningOrVariantCreationTrueBasedOnAddDeleteVariantSwitch(ProductAttribute productAttribute) {
    return (AttributeType.DEFINING_ATTRIBUTE.name().equalsIgnoreCase(productAttribute.getAttributeType())
        || productAttribute.isVariantCreation()) && !addDeleteVariantSwitch;
  }

  public String getKeyBasedOnAttributeType(ProductAttribute productAttribute) {
    if (AttributeType.DEFINING_ATTRIBUTE.name().equalsIgnoreCase(productAttribute.getAttributeType())
        || productAttribute.isVariantCreation()) {
      return productAttribute.getAttributeCode() + Constants.HYPHEN + productAttribute.getValue();
    }
    return productAttribute.getAttributeCode();
  }

  public void initializeProductDetailsWithMFDTrue(Product product) {
    Hibernate.initialize(product.getProductAttributes());
    Hibernate.initialize(product.getProductImages());
    Hibernate.initialize(product.getProductItems());
    Iterator<ProductItem> productItemIterator = product.getProductItems().iterator();
    while (productItemIterator.hasNext()) {
      ProductItem productItem = productItemIterator.next();
      Hibernate.initialize(productItem.getProductItemAttributes());
      Hibernate.initialize(productItem.getProductItemImages());
    }
  }

  public void initializeAllProductDetails(Product product) {
    Hibernate.initialize(product.getProductItems());
    initializeProductItems(product);
    Hibernate.initialize(product.getProductAttributes());
    Hibernate.initialize(product.getProductImages());
    Iterator<ProductImage> productImageIterator = product.getProductImages().iterator();
    while (productImageIterator.hasNext()) {
      ProductImage productImage = productImageIterator.next();
      if (productImage.isMarkForDelete()) {
        productImageIterator.remove();
      }
    }
    Iterator<ProductAttribute> productAttributeIterator = product.getProductAttributes().iterator();
    while (productAttributeIterator.hasNext()) {
      ProductAttribute productAttribute = productAttributeIterator.next();
      if (productAttribute.isMarkForDelete()) {
        productAttributeIterator.remove();
      }
    }
  }

  private void initializeProductItems(Product product) {
    Iterator<ProductItem> productItemIterator = product.getProductItems().iterator();
    while (productItemIterator.hasNext()) {
      ProductItem productItem = productItemIterator.next();
      if (productItem.isMarkForDelete()) {
        productItemIterator.remove();
      } else {
        Hibernate.initialize(productItem.getProductItemAttributes());
        Hibernate.initialize(productItem.getProductItemImages());
        Iterator<ProductItemImage> productItemImageIterator =
            productItem.getProductItemImages().iterator();
        while (productItemImageIterator.hasNext()) {
          ProductItemImage productItemImage = productItemImageIterator.next();
          if (productItemImage.isMarkForDelete()) {
            productItemImageIterator.remove();
          }
        }
        Iterator<ProductItemAttribute> productItemAttributeIterator =
            productItem.getProductItemAttributes().iterator();
        while (productItemAttributeIterator.hasNext()) {
          ProductItemAttribute productItemAttribute = productItemAttributeIterator.next();
          if (productItemAttribute.isMarkForDelete()) {
            productItemAttributeIterator.remove();
          }
        }
      }
    }
  }

  public List<ProductHistoryDTO> getImageChanges(Product existingProduct, Product newProduct) {
    List<ProductHistoryDTO> productImageHistoryList = new ArrayList<>();
    productImageHistoryList.addAll(addProductImagesChanges(existingProduct, newProduct));
    Map<String, ProductItemImageChangeHistoryDto> existingItemToImageMapping =
        getProductItemToImageMapping(existingProduct);
    Map<String, ProductItemImageChangeHistoryDto> newItemToImageMapping =
        getProductItemToImageMapping(newProduct);
    productImageHistoryList.addAll(addProductItemImagesChanges(existingItemToImageMapping,
        newItemToImageMapping));
    return productImageHistoryList;
  }

  private List<ProductHistoryDTO> addProductItemImagesChanges(
      Map<String, ProductItemImageChangeHistoryDto> existingItemToImageMapping,
      Map<String, ProductItemImageChangeHistoryDto> newItemToImageMapping) {
    List<ProductHistoryDTO> productItemImageHistoryList = new ArrayList<>();
    for (Map.Entry<String, ProductItemImageChangeHistoryDto> entry : newItemToImageMapping.entrySet()) {
      final ProductItemImageChangeHistoryDto existingPair = existingItemToImageMapping.get(entry.getKey());
      if (Objects.nonNull(existingPair)) {
        if (!String.valueOf(entry.getValue().getMainImage()).equals(String.valueOf(existingPair.getMainImage()))
            && entry.getValue().getOriginalImage() == existingPair.getOriginalImage()) {
          productItemImageHistoryList.add(
              ProductHistoryDTO.builder().skuName(entry.getKey()).field(Constants.PRODUCT_MAIN_IMAGE)
                  .oldValue(existingPair.getMainImage()).newValue(entry.getValue().getMainImage()).build());
        }
        if (!entry.getValue().getImages().equals(existingPair.getImages())) {
          productItemImageHistoryList.add(
              ProductHistoryDTO.builder().skuName(entry.getKey()).field(Constants.PRODUCT_IMAGES)
                  .oldValue(existingPair.getImages().subSequence(1, existingPair.getImages().length() - 1).toString())
                  .newValue(
                      entry.getValue().getImages().subSequence(1, entry.getValue().getImages().length() - 1).toString())
                  .build());
        }
      }
    }
    return productItemImageHistoryList;
  }

  private Map<String, ProductItemImageChangeHistoryDto> getProductItemToImageMapping(Product product) {
    Map<String, ProductItemImageChangeHistoryDto> itemToImageMap = new HashMap<>();
    for (ProductItem productItem : product.getProductItems()) {
      String mainImage = null;
      Boolean originalImage = null;
      Set<String> images = new TreeSet<>();
      for (ProductItemImage productItemImage : productItem.getProductItemImages()) {
        if (productItemImage.getMainImage()) {
          mainImage = productItemImage.getLocationPath();
          originalImage = productItemImage.getOriginalImage();
        }
        images.add(productItemImage.getLocationPath());
      }
      itemToImageMap.put(productItem.getGeneratedItemName(),
          new ProductItemImageChangeHistoryDto(originalImage, mainImage, images.toString()));
    }
    return itemToImageMap;
  }

  private List<ProductHistoryDTO> addProductImagesChanges(Product existingProduct,
      Product newProduct) {
    List<ProductHistoryDTO> productImageHistoryList = new ArrayList<>();
    Set<String> productImages = existingProduct.getProductImages()
        .parallelStream()
        .map(productImage -> productImage.getLocationPath())
        .collect(Collectors.toSet());
    String oldValue =
        productImages.toString().subSequence(1, productImages.toString().length() - 1).toString();
    List<String> newProductImages = new ArrayList<>();
    newProduct.getProductImages().stream().forEach(productImage -> {
      newProductImages.add(productImage.getLocationPath());
      productImages.remove(productImage.getLocationPath());
    });
    if (org.apache.commons.collections.CollectionUtils.isNotEmpty(productImages)) {
      productImageHistoryList.add(ProductHistoryDTO.builder()
          .field(Constants.PRODUCT_IMAGES)
          .newValue(newProductImages.toString()
              .subSequence(1, newProductImages.toString().length() - 1)
              .toString())
          .oldValue(oldValue)
          .build());
    }
    return productImageHistoryList;
  }

  public List<ProductHistoryDTO> getProductDetailChanges(Product existingProduct,
      Product newProduct) {
    List<ProductHistoryDTO> productHistoryDTOList = new ArrayList<>();
    String oldValue = Optional.ofNullable(existingProduct.getVideoUrl()).orElse("");
    String newValue = Optional.ofNullable(newProduct.getVideoUrl()).orElse("");
    if (!oldValue.equals(newValue)) {
      productHistoryDTOList.add(ProductHistoryDTO.builder()
          .field(Constants.PRODUCT_URL)
          .newValue(newValue)
          .oldValue(oldValue)
          .build());
    }
    newValue = new String(newProduct.getDescription());
    oldValue = new String(existingProduct.getDescription());
    if (!oldValue.equals(newValue)) {
      productHistoryDTOList.add(ProductHistoryDTO.builder()
          .field(Constants.PRODUCT_DESCRIPTION)
          .newValue(newValue)
          .oldValue(oldValue)
          .build());
    }
    if (!existingProduct.getProductName().equals(newProduct.getProductName())) {
      productHistoryDTOList.add(ProductHistoryDTO.builder()
          .field(Constants.PRODUCT_NAME)
          .newValue(newProduct.getProductName())
          .oldValue(existingProduct.getProductName())
          .build());
    }
    if (Objects.isNull(existingProduct.getUniqueSellingPoint())) {
      if (StringUtils.isNotEmpty(newProduct.getUniqueSellingPoint())) {
        productHistoryDTOList.add(
            ProductHistoryDTO.builder().field(Constants.PRODUCT_USP).newValue(newProduct.getUniqueSellingPoint())
                .oldValue(StringUtils.EMPTY).build());
      }
    } else if (!existingProduct.getUniqueSellingPoint().equals(newProduct.getUniqueSellingPoint())) {
      productHistoryDTOList.add(ProductHistoryDTO.builder()
          .field(Constants.PRODUCT_USP)
          .newValue(newProduct.getUniqueSellingPoint())
          .oldValue(existingProduct.getUniqueSellingPoint())
          .build());
    }
    if (Objects.isNull(newProduct.getUniqueSellingPoint())) {
      newProduct.setUniqueSellingPoint(StringUtils.EMPTY);
    }
    if (!existingProduct.getCategoryCode().equals(newProduct.getCategoryCode())) {
      productHistoryDTOList.add(ProductHistoryDTO.builder()
          .field(Constants.CATEGORY)
          .newValue(newProduct.getCategoryName())
          .oldValue(existingProduct.getCategoryName())
          .build());
    }
    if (!existingProduct.getProductItems()
        .get(0)
        .getDangerousGoodsLevel()
        .equals(newProduct.getProductItems().get(0).getDangerousGoodsLevel())) {
      productHistoryDTOList.add(ProductHistoryDTO.builder()
          .field(Constants.PRODUCT_DG_LEVEL)
          .newValue(newProduct.getProductItems().get(0).getDangerousGoodsLevel().toString())
          .oldValue(existingProduct.getProductItems().get(0).getDangerousGoodsLevel().toString())
          .build());
    }
    addProductAttributeChanges(existingProduct, newProduct, productHistoryDTOList);
    addProductItemAttributeChanges(existingProduct, newProduct, productHistoryDTOList);
    if (!Objects.equals(existingProduct.getLength(), newProduct.getLength())) {
      productHistoryDTOList.add(
          ProductHistoryDTO.builder().field(Constants.LENGHT).newValue(newProduct.getLength().toString())
              .oldValue(existingProduct.getLength().toString()).build());
    }
    if (!Objects.equals(existingProduct.getWidth(), newProduct.getWidth())) {
      productHistoryDTOList.add(
          ProductHistoryDTO.builder().field(Constants.WIDTH).newValue(newProduct.getWidth().toString())
              .oldValue(existingProduct.getWidth().toString()).build());
    }
    if (!Objects.equals(existingProduct.getHeight(), newProduct.getHeight())) {
      productHistoryDTOList.add(
          ProductHistoryDTO.builder().field(Constants.HEIGHT).newValue(newProduct.getHeight().toString())
              .oldValue(existingProduct.getHeight().toString()).build());
    }
    if (!Objects.equals(existingProduct.getWeight(), newProduct.getWeight())) {
      productHistoryDTOList.add(
          ProductHistoryDTO.builder().field(Constants.WEIGHT).newValue(newProduct.getWeight().toString())
              .oldValue(existingProduct.getWeight().toString()).build());
    }
    if (Objects.nonNull(existingProduct.getProductType()) && Objects.nonNull(newProduct.getProductType())) {
      if (!Objects.equals(existingProduct.getProductType(), newProduct.getProductType())) {
        productHistoryDTOList.add(
            ProductHistoryDTO.builder().field(Constants.PRODUCT_TYPE).newValue(newProduct.getProductType().toString())
                .oldValue(existingProduct.getProductType().toString()).build());
      }
    }
    return productHistoryDTOList;
  }

  public String toJson(Object o) throws JsonProcessingException {
    return this.objectMapper.writeValueAsString(o);
  }

  public boolean validateYouTubeUrl(String youTubeUrl, YouTube youTube) throws Exception {
    try {
      String[] apiKeys = youTubeDataApiKey.split(COMMA);
      int apiKeyIndex = (int) (Math.random() * apiKeys.length);
      String youTubeApiKey = apiKeys[apiKeyIndex];
      String videoId = validateUrlAndGetVideoId(VIDEO_PATTERN, youTubeUrl);
      if (Objects.nonNull(videoId) && Objects.nonNull(validateUrlAndGetVideoId(YOUTUBE_PATTERN, youTubeUrl))) {
        log.debug("Validating youtube videoId: {} using api key index: {}", videoId, apiKeyIndex);
        return isYouTubeUrlActive(videoId, youTubeApiKey, youTube);
      }
    } catch (Exception e) {
      log.error("getting exception while calling youtube API for url: {} ", youTubeUrl, e);
    }
    return false;
  }

  public boolean validateProtectedBrand(String brandCode, String sellerCode) throws Exception {
    try {
      return productServiceRepository.validateProtectedBrand(brandCode, sellerCode);
    } catch (Exception e) {
      log.error("Exception caught while validating protected brand , code : {} , sellerCode : {} ", brandCode, sellerCode, e);
      return false;
    }
  }

  public String getBrandCodeByBrandName(String brandName) {
    try {
      return productServiceRepository.filterByBrandName(brandName).getBrandCode();
    } catch (Exception e) {
      log.error("Exception caught while getting brand code by name : {} ", brandName, e);
      return StringUtils.EMPTY;
    }
  }

  private void addProductItemAttributeChanges(Product existingProduct,
      Product newProduct, List<ProductHistoryDTO> productHistoryDTOList) {
    for(ProductItem productItem : newProduct.getProductItems()) {
      ProductItem existingItem = existingProduct.getProductItems().parallelStream()
          .filter(productItem1 -> productItem1.getSkuCode().equals(productItem.getSkuCode()))
          .findAny().get();
      if(Objects.nonNull(existingItem)) {
        Map<String, String> itemToAttrMap = new HashMap<>();
        for (ProductItemAttribute productItemAttribute : productItem.getProductItemAttributes()) {
          itemToAttrMap.put(productItemAttribute.getAttributeCode(), productItemAttribute.getValue());
        }

        Map<String, String> itemToAttrMapExistingAttributes = new HashMap<>();
        for (ProductItemAttribute productItemAttribute : existingItem.getProductItemAttributes()) {
          if (!itemToAttrMapExistingAttributes.containsKey(productItemAttribute.getAttributeCode())) {
            itemToAttrMapExistingAttributes
                .put(productItemAttribute.getAttributeCode(), productItemAttribute.getValue());
          }
        }
        for (ProductItemAttribute productItemAttribute : existingItem.getProductItemAttributes()) {
          if (!Constants.BRAND.equalsIgnoreCase(productItemAttribute.getName())) {
            if (itemToAttrMap.containsKey(productItemAttribute.getAttributeCode()) && Objects
                .nonNull(productItemAttribute.getValue()) && !productItemAttribute.getValue()
                .equals(itemToAttrMap.get(productItemAttribute.getAttributeCode()))) {
              productHistoryDTOList.add(ProductHistoryDTO.builder().skuName(productItem.getGeneratedItemName())
                  .field(Constants.HISTORY_ATTRIBUTE + productItemAttribute.getName())
                  .newValue(itemToAttrMap.get(productItemAttribute.getAttributeCode()))
                  .oldValue(productItemAttribute.getValue()).build());
            } else if (!itemToAttrMap.containsKey(productItemAttribute.getAttributeCode())) {
              productHistoryDTOList.add(ProductHistoryDTO.builder().skuName(productItem.getGeneratedItemName())
                  .field(Constants.HISTORY_DELETED_ATTRIBUTE + productItemAttribute.getName()).newValue(null)
                  .oldValue(productItemAttribute.getValue()).build());
            }
          }
        }
        productHistoryDTOList.addAll(productItem.getProductItemAttributes().parallelStream()
            .filter(productItemAttribute -> !itemToAttrMapExistingAttributes.containsKey(productItemAttribute.getAttributeCode()))
            .map(productAttribute -> ProductHistoryDTO.builder()
                .skuName(productItem.getGeneratedItemName())
                .field(Constants.HISTORY_ADDED_ATTRIBUTE + productAttribute.getName())
                .oldValue(null)
                .newValue(productAttribute.getValue())
                .build())
            .collect(Collectors.toList()));
      }
    }
  }

  private void addProductAttributeChanges(Product existingProduct,
      Product newProduct,
      List<ProductHistoryDTO> productHistoryDTOList) {
    Map<String,String> productAttributeCodeToValue = new HashMap<>();
    for(ProductAttribute productAttribute : existingProduct.getProductAttributes()){
      if(Objects.nonNull(productAttribute)){
        productAttributeCodeToValue.put(productAttribute.getAttributeCode(),productAttribute.getValue());
      }
    }
    Map<String, String> newProductAttributeCodeToValue = new HashMap<>();
    for(ProductAttribute productAttribute : newProduct.getProductAttributes()){
      if(Objects.nonNull(productAttribute)){
        newProductAttributeCodeToValue.put(productAttribute.getAttributeCode(),productAttribute.getValue());
      }
    }

    productHistoryDTOList.addAll(newProduct.getProductAttributes()
        .parallelStream()
        .filter(productAttribute ->
            (AttributeType.DESCRIPTIVE_ATTRIBUTE.toString().equals(productAttribute.getAttributeType())
                || AttributeType.PREDEFINED_ATTRIBUTE.toString().equals(productAttribute.getAttributeType()))
                && !productAttribute.isVariantCreation()
                && productAttributeCodeToValue.containsKey(productAttribute.getAttributeCode())
            && !productAttribute.getValue().equals(productAttributeCodeToValue.get(productAttribute.getAttributeCode())))
        .map(productAttribute -> ProductHistoryDTO.builder()
            .field(Constants.HISTORY_ATTRIBUTE + productAttribute.getName())
            .oldValue(productAttributeCodeToValue.get(productAttribute.getAttributeCode()))
            .newValue(productAttribute.getValue())
            .build())
        .collect(Collectors.toList()));
    productHistoryDTOList.addAll(newProduct.getProductAttributes().
        parallelStream().filter(productAttribute -> !productAttributeCodeToValue.containsKey(productAttribute.getAttributeCode()))
        .map(productAttribute -> ProductHistoryDTO.builder()
            .field(Constants.HISTORY_ADDED_ATTRIBUTE + productAttribute.getName())
            .oldValue(null)
            .newValue(productAttribute.getValue())
            .build())
        .collect(Collectors.toList()));
    productHistoryDTOList.addAll(existingProduct.getProductAttributes().
        parallelStream().filter(productAttribute -> !newProductAttributeCodeToValue.containsKey(productAttribute.getAttributeCode()))
        .map(productAttribute -> ProductHistoryDTO.builder()
            .field(Constants.HISTORY_DELETED_ATTRIBUTE + productAttribute.getName())
            .oldValue(productAttribute.getValue())
            .newValue(null)
            .build())
        .collect(Collectors.toList()));
  }

  private boolean isYouTubeUrlActive(String videoID, String apiKey, YouTube youTube) throws IOException {
    YouTube.Videos.List listVideosRequest = youTube.videos().list(VIDEO_LIST);
    listVideosRequest.setId(videoID);
    listVideosRequest.setKey(apiKey);
    VideoListResponse listResponse = listVideosRequest.execute();
    log.info("getting youtube api response: {}", listResponse);
    return listResponse.getPageInfo().getTotalResults() >= 1;
  }

  private String validateUrlAndGetVideoId(Pattern pattern, String youtubeUrl) {
    Matcher matcher = pattern.matcher(youtubeUrl);
    if (matcher.find()) {
      return matcher.group();
    }
    return null;
  }

  public void removeOriginalImagesFromProduct(Product product) {
    product.getProductImages().removeIf(productImage -> Boolean.TRUE.equals(productImage.getOriginalImage()));
    product.getProductItems().stream().forEach(productItem -> productItem.getProductItemImages()
        .removeIf(productItemImage -> Boolean.TRUE.equals(productItemImage.getOriginalImage())));
  }

  public void setProductDimensionsAndProductTypeAndDgLevel(Product product,
      PDTDimensionRefreshEventModel pdtDimensionRefreshEventModel) {
    //set dimension details
    product.setLength(pdtDimensionRefreshEventModel.getLength());
    product.setWidth(pdtDimensionRefreshEventModel.getWidth());
    product.setHeight(pdtDimensionRefreshEventModel.getHeight());
    product.setWeight(pdtDimensionRefreshEventModel.getWeight());
    product.setShippingWeight(pdtDimensionRefreshEventModel.getShippingWeight());

    if (Objects.nonNull(pdtDimensionRefreshEventModel.getProductType())) {
      product.setProductType(pdtDimensionRefreshEventModel.getProductType());
    }

    //set dg level in item
    if (Objects.nonNull(pdtDimensionRefreshEventModel.getDangerousGoodsLevel())) {
      product.getProductItems().forEach(productItem -> productItem.setDangerousGoodsLevel(
        pdtDimensionRefreshEventModel.getDangerousGoodsLevel()));
    }
  }

  public void setCommonImageFlagForProductAndItemImages(Product product) {
    Map<String, List<ProductItemImage>> imagePathAndItemImageMap =
        product.getProductItems().stream().flatMap(productItem -> productItem.getProductItemImages().stream())
            .collect(Collectors.groupingBy(ProductItemImage::getLocationPath));
    for (ProductImage productImage : product.getProductImages()) {
      List<ProductItemImage> productItemImages = imagePathAndItemImageMap.get(productImage.getLocationPath());
      if (!CollectionUtils.isEmpty(productItemImages) && productItemImages.size() == product.getProductItems().size()) {
        setCommonImageFlag(productImage, productItemImages, true);
      } else {
        setCommonImageFlag(productImage, productItemImages, false);
      }
    }
  }

  private void setCommonImageFlag(ProductImage productImage, List<ProductItemImage> productItemImages,
      boolean commonImage) {
    productImage.setCommonImage(commonImage);
    if (!CollectionUtils.isEmpty(productItemImages)) {
      productItemImages.forEach(productItemImage -> productItemImage.setCommonImage(commonImage));
    }
  }

  public boolean isProductCode(String productCode) {
    Pattern pattern = Pattern.compile(vendorSearchAutoHealRegex);
    Matcher matcher = pattern.matcher(productCode);
    return matcher.matches();
  }

  public void copyProductItemAttributesFromPCB(ProductItem productItemPcb, ProductItem productItem, boolean isItemDelete) {
    List<ProductItemAttribute> productItemAttributesFromPCB = new ArrayList<>(productItemPcb.getProductItemAttributes());
    productItem.setProductItemAttributes(productItemAttributesFromPCB);
    productItemAttributesFromPCB.forEach(productItemAttribute -> {
      productItemAttribute.setId(null);
      productItemAttribute.setProduct(isItemDelete ? productItemPcb : productItem);
    });
  }

  public boolean productContainsEmptyItemAttributes(Product product) {
    // If none of the Item Images are MFD false in PDT , proceed with auto heal
    return product.getProductItems().stream().anyMatch(
        productItem -> org.apache.commons.collections.CollectionUtils.isEmpty(productItem.getProductItemAttributes())
            || product.getProductItems().stream().filter(
                productItems -> org.apache.commons.collections.CollectionUtils.isNotEmpty(
                    productItems.getProductItemAttributes()))
            .flatMap(productItems -> productItem.getProductItemAttributes().stream())
            .noneMatch(Predicate.not(ProductItemAttribute::isMarkForDelete)));
  }
}

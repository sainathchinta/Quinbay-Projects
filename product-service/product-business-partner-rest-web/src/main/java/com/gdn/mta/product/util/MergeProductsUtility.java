package com.gdn.mta.product.util;


import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.gdn.mta.product.entity.MergeStatus;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.dto.response.ProductItemAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemImage;

/**
 * Created by shripati on 22/03/16.
 */
public class MergeProductsUtility {

  private static final Logger LOG = LoggerFactory.getLogger(MergeProductsUtility.class);
  private Product masterDataProduct;
  private Product duplicateProduct;
  private String errorInMerge;

  public MergeProductsUtility(Product masterDataProduct, Product duplicateProduct) {
    this.masterDataProduct = masterDataProduct;
    this.duplicateProduct = duplicateProduct;
  }

  public MergeStatus merge(Boolean forceMerge) {
    // Validate if product categories match
    LOG.debug("Merging Master : {}, Duplicate : {} , force Merge: {}", masterDataProduct.getId(),
        duplicateProduct.getId(), forceMerge);
    MergeStatus status = MergeStatus.NO_UPDATE;
    if (!forceMerge) {
      List<ProductAttribute> masterProductAttributes = masterDataProduct.getProductAttributes();
      Map<String, ProductAttribute> mapOfMasterProductAttr =
          convertProductAttrToMap(masterProductAttributes);
      List<ProductAttribute> duplicateProductAttributes = duplicateProduct.getProductAttributes();
      for (ProductAttribute dupAttr : duplicateProductAttributes) {
        if (dupAttr.getAttribute().isSkuValue() || dupAttr.isMarkForDelete()) {
          continue;
        }
        ProductAttribute masterAttr = mapOfMasterProductAttr.get(dupAttr.getAttribute().getId());
        LOG.debug("masterAttr : {}, DuplicateAttr : {} ", masterAttr,
            dupAttr.getAttribute().getName());
        if (masterAttr == null) {
          LOG.error(
              "Cannot merge as product attributes are different : Master Product : {} , Duplicate "
                  + "Product : {}", masterDataProduct.getId(), duplicateProduct.getId());
          errorInMerge = "Attribute " + dupAttr.getProductAttributeName()
              + " does not exist in master product";
          return MergeStatus.INVALID;
        }
      }
      LOG.debug("Going to compare defining attributes. These lead to new product items");
      for (ProductAttribute dupAttr : duplicateProductAttributes) {
        if (dupAttr.isMarkForDelete() || dupAttr.getAttribute().isSkuValue()) {
          continue;
        }
        ProductAttribute masterAttr = mapOfMasterProductAttr.get(dupAttr.getAttribute().getId());
        if (masterAttr.getAttribute().getAttributeType().equals(AttributeType.DEFINING_ATTRIBUTE)) {
          Map<String, ProductAttributeValue> uniqueProductAttributes = new HashMap<>();
          appendUniqueProductAttr(uniqueProductAttributes, masterAttr.getProductAttributeValues());
          LOG.debug("Master attribute values before merge : {} ", uniqueProductAttributes);
          int prodAttrSizeBefore = uniqueProductAttributes.size();
          appendUniqueProductAttr(uniqueProductAttributes, dupAttr.getProductAttributeValues());
          LOG.debug("Master attribute values after merging duplicates : {} ",
              uniqueProductAttributes);
          int prodAttrSizeAfter = uniqueProductAttributes.size();
          if (prodAttrSizeAfter != prodAttrSizeBefore) {
            if (masterDataProduct.getProductAttributes().contains(masterAttr)) {
              masterDataProduct.getProductAttributes().remove(masterAttr);
            }
            masterAttr.setProductAttributeValues(new ArrayList<>(uniqueProductAttributes.values()));
            masterDataProduct.getProductAttributes().add(masterAttr);
            status = MergeStatus.MASTER_DATA_UPDATE;

          }

        }
      }
    }
    List<ProductItemImage> defaultProductImages = new ArrayList<>();
    for(ProductImage productImage : masterDataProduct.getProductImages()){
      if (!productImage.isMarkForDelete()) {
        ProductItemImage productItemImage = new ProductItemImage();
        productItemImage.setStoreId(productImage.getStoreId());
        productItemImage.setLocationPath(productImage.getLocationPath());
        productItemImage.setMainImages(productImage.isMainImages());
        productItemImage.setSequence(productImage.getSequence());
        defaultProductImages.add(productItemImage);
      }
    }
    for(ProductItem productItem : duplicateProduct.getProductItems()){
      productItem.setProduct(masterDataProduct);
      masterDataProduct.getProductItems().add(productItem);
        productItem.setProductItemImages(defaultProductImages);
      if (!productItem.isMarkForDelete()) {
        productItem.setViewable(true);
        productItem.setActivated(true);
      }
    }
    LOG.debug("Returning merge required for request : {} ", status);
    return status;
  }

  private void appendUniqueProductAttr(Map<String, ProductAttributeValue> uniqueProductAttributes,
      List<ProductAttributeValue> productAttributeValues) {

    for (ProductAttributeValue productAttributeValue : productAttributeValues) {
      if (!productAttributeValue.isMarkForDelete()) {
        uniqueProductAttributes.put(productAttributeValue.getAllowedAttributeValue()
                .getValue(),
            productAttributeValue);
      }
    }
  }

  private Map<String, ProductAttribute> convertProductAttrToMap(
      List<ProductAttribute> masterProductAttributes) {
    Map<String, ProductAttribute> mapOfAttr = new HashMap<>();
    for (ProductAttribute prodAttr : masterProductAttributes) {
      mapOfAttr.put(prodAttr.getAttribute().getId(), prodAttr);
    }
    return mapOfAttr;
  }

  private List<String> getProductAttrValues(List<ProductAttributeValue> productAttributeValues) {
    List<String> listOfAttrValues = new ArrayList<>();
    for (ProductAttributeValue productAttributeValue : productAttributeValues) {
      if (productAttributeValue.getDescriptiveAttributeValueType() != null) {
        switch (productAttributeValue.getDescriptiveAttributeValueType()) {
          case SINGLE:
            listOfAttrValues.add(
                DescriptiveAttributeValueType.SINGLE + ":" + StringUtils
                    .lowerCase(StringUtils.strip(productAttributeValue
                        .getDescriptiveAttributeValue())));
            break;
          case PREDEFINED:
            listOfAttrValues.add(
                DescriptiveAttributeValueType.PREDEFINED + ":" + StringUtils.lowerCase(
                    StringUtils.strip(productAttributeValue.getPredefinedAllowedAttributeValue()
                        .getValue())));
            break;
          default:
            break;
        }
      }
    }
    return listOfAttrValues;
  }

  public Map<String, String> getOldToNewProductItemIdMap(
      List<ProductItemResponse> newMasterProductItems,
      List<ProductItemResponse> duplicateProductItems) {
    List<Map<String, String>> masterProductItemsList = new ArrayList<>();
    for (ProductItemResponse masterProductItem : newMasterProductItems) {
      Map<String, String> masterProductItemAttrs = getProductItemToMap(masterProductItem);
      masterProductItemsList.add(masterProductItemAttrs);
    }

    Map<String, String> oldToNewProductItemMap = new HashMap<>();
    for (ProductItemResponse dupProductItem : duplicateProductItems) {
      Map<String, String> duplicateProductItemAttrs = getProductItemToMap(dupProductItem);
      for (Map<String, String> masterProductItemAtts : masterProductItemsList) {
        boolean allDupToMasterAttrMatch = true;
        for (Map.Entry<String, String> duplicateProdItemAttr : duplicateProductItemAttrs
            .entrySet()) {
          if (!"PRODUCT_ITEM_ID".equals(duplicateProdItemAttr.getKey())) {
            String itemAttrVal =
                masterProductItemAtts.get(duplicateProdItemAttr.getKey());
            allDupToMasterAttrMatch =
                allDupToMasterAttrMatch & duplicateProdItemAttr.getValue()
                    .equals(itemAttrVal);
            if (!allDupToMasterAttrMatch) {
              break;
            }
          }
        }

        if (allDupToMasterAttrMatch) {
          oldToNewProductItemMap.put(duplicateProductItemAttrs.get("PRODUCT_ITEM_ID"),
              masterProductItemAtts.get("PRODUCT_ITEM_ID"));
          break;
        }
      }
    }
    return oldToNewProductItemMap;
  }

  private Map<String, String> getProductItemToMap(ProductItemResponse productItem) {
    Map<String, String> productItemAttrs = new HashMap<>();
    for (ProductItemAttributeValueResponse itemAttrValue : productItem
        .getProductItemAttributeValueResponses()) {
      productItemAttrs
          .put(itemAttrValue.getAttributeResponse().getId(), itemAttrValue.getValue());
    }
    productItemAttrs.put("PRODUCT_ITEM_ID", productItem.getId());
    return productItemAttrs;
  }

  public String getErrorInMerge() {
    return errorInMerge;
  }
}

package com.gdn.mta.bulk.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import org.apache.commons.lang3.StringUtils;

import com.gdn.mta.bulk.MerchantStatusType;
import com.gdn.mta.bulk.helper.ExcelTemplateUtil;
import com.gdn.partners.bulk.util.BulkCnCreationHeaderNames;
import com.gdn.partners.bulk.util.CnExcelHeaderInfo;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.bulk.util.ExcelHeaderNames;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;

public class CnBulkHeaders {

  private static boolean isDescriptiveOrPredefinedAndNotVariantCreation(CategoryAttributeResponse categoryAttributeResponse) {
    return (AttributeType.DESCRIPTIVE_ATTRIBUTE.name().equals(categoryAttributeResponse.getAttribute().getAttributeType())
        || AttributeType.PREDEFINED_ATTRIBUTE.name().equals(categoryAttributeResponse.getAttribute().getAttributeType()))
        && !categoryAttributeResponse.getAttribute().isVariantCreation();
  }

  private static boolean isDefiningOrVariantCreation(CategoryAttributeResponse categoryAttributeResponse) {
    return AttributeType.DEFINING_ATTRIBUTE.name().equals(categoryAttributeResponse.getAttribute().getAttributeType())
        || categoryAttributeResponse.getAttribute().isVariantCreation();
  }

  private static void addImagesEn(List<String> primaryHeaderList, List<String> secondaryHeaderList) {
    for (int i = 0; i < Constant.MAX_IMAGE_COUNT ; i++) {
      if (i == 0) {
        primaryHeaderList.add(ExcelHeaderNames.IMAGE_PREFIX + (i + 1));
        secondaryHeaderList.add(CnExcelHeaderInfo.IMAGES_EN);
      } else {
        primaryHeaderList.add(ExcelHeaderNames.IMAGE_PREFIX + (i + 1));
        secondaryHeaderList.add(StringUtils.EMPTY);
      }
    }
    primaryHeaderList.add(ExcelHeaderNames.URL_VIDEO);
    secondaryHeaderList.add(CnExcelHeaderInfo.URL_VIDEO_EN);
  }

  private static void addShipmentInfoEn(List<String> primaryHeaderList, List<String> secondaryHeaderList) {
    primaryHeaderList.addAll(Arrays.asList(ExcelHeaderNames.HANDLING_TYPE, ExcelHeaderNames.PICKUP_POINT_CODE,
        ExcelHeaderNames.LENGTH, ExcelHeaderNames.WIDTH, ExcelHeaderNames.HEIGHT, ExcelHeaderNames.WEIGHT));
    secondaryHeaderList.addAll(Arrays.asList(CnExcelHeaderInfo.HANDLING_TYPE_EN, CnExcelHeaderInfo.PICKUP_POINT_CODE_EN,
        CnExcelHeaderInfo.LENGTH_EN, CnExcelHeaderInfo.WIDTH_EN, CnExcelHeaderInfo.HEIGHT_EN, CnExcelHeaderInfo.WEIGHT_EN));
  }

  private static void addPriceAndStockEn(List<String> primaryHeaderList, List<String> secondaryHeaderList) {
    primaryHeaderList.addAll(Arrays.asList(ExcelHeaderNames.PRICE, ExcelHeaderNames.SELLING_PRICE,
        ExcelHeaderNames.AVAILABLE_STOCK, ExcelHeaderNames.MINIMUM_STOCK));
    secondaryHeaderList.addAll(Arrays.asList(CnExcelHeaderInfo.PRICE_EN, CnExcelHeaderInfo.SELLING_PRICE_EN,
        CnExcelHeaderInfo.AVAILABLE_STOCK_EN, CnExcelHeaderInfo.MINIMUM_STOCK_EN));
    }

  private static void addSkuVisibilityEn(List<String> primaryHeaderList, List<String> secondaryHeaderList,
      MerchantStatusType merchantStatusType) {
    switch (merchantStatusType){
      case PURE_DELIVERY:{
        primaryHeaderList.add(ExcelHeaderNames.DELIVERY_STATUS);
        secondaryHeaderList.add(CnExcelHeaderInfo.DELIVERY_STATUS_EN);
        break;
      }
      case DELIVERY_AND_CNC:{
        primaryHeaderList.add(ExcelHeaderNames.DELIVERY_STATUS);
        secondaryHeaderList.add(CnExcelHeaderInfo.DELIVERY_STATUS_EN);
        primaryHeaderList.add(ExcelHeaderNames.CNC_STATUS);
        secondaryHeaderList.add(CnExcelHeaderInfo.CNC_STATUS_EN);
      break;
      }
      case BFB:{
        primaryHeaderList.add(ExcelHeaderNames.BFB_BASE_PRICE);
        primaryHeaderList.add(ExcelHeaderNames.BFB_MANAGED_INFO);
        primaryHeaderList.add(ExcelHeaderNames.BFB_STATUS);
        primaryHeaderList.add(ExcelHeaderNames.DELIVERY_STATUS);
        secondaryHeaderList.add(CnExcelHeaderInfo.SECONDARY_HEADER_BFB_BASE_PRICE_EN);
        secondaryHeaderList.add(CnExcelHeaderInfo.SECONDARY_HEADER_BFB_MANAGED_INFO_EN);
        secondaryHeaderList.add(CnExcelHeaderInfo.SECONDARY_HEADER_BFB_STATUS_EN);
        secondaryHeaderList.add(CnExcelHeaderInfo.DELIVERY_STATUS_EN);
        break;
      }
      case BFB_AND_CNC: {
        primaryHeaderList.add(ExcelHeaderNames.BFB_BASE_PRICE);
        primaryHeaderList.add(ExcelHeaderNames.BFB_MANAGED_INFO);
        primaryHeaderList.add(ExcelHeaderNames.BFB_STATUS);
        primaryHeaderList.add(ExcelHeaderNames.DELIVERY_STATUS);
        primaryHeaderList.add(ExcelHeaderNames.CNC_STATUS);
        secondaryHeaderList.add(CnExcelHeaderInfo.SECONDARY_HEADER_BFB_BASE_PRICE_EN);
        secondaryHeaderList.add(CnExcelHeaderInfo.SECONDARY_HEADER_BFB_MANAGED_INFO_EN);
        secondaryHeaderList.add(CnExcelHeaderInfo.SECONDARY_HEADER_BFB_STATUS_EN);
        secondaryHeaderList.add(CnExcelHeaderInfo.DELIVERY_STATUS_EN);
        secondaryHeaderList.add(CnExcelHeaderInfo.CNC_STATUS_EN);
        break;
      }
      default:{
        break;
      }
    }
  }

  private static void addImagesIn(List<String> primaryHeaderList, List<String> secondaryHeaderList) {
    for (int i = 0; i < Constant.MAX_IMAGE_COUNT ; i++) {
      if (i == 0) {
        primaryHeaderList.add(ExcelHeaderNames.FOTO_PREFIX + (i + 1));
        secondaryHeaderList.add(CnExcelHeaderInfo.FOTO_IN);
      } else {
        primaryHeaderList.add(ExcelHeaderNames.FOTO_PREFIX + (i + 1));
        secondaryHeaderList.add(StringUtils.EMPTY);
      }
    }
    primaryHeaderList.add(ExcelHeaderNames.URL_VIDEO);
    secondaryHeaderList.add(CnExcelHeaderInfo.URL_VIDEO_IN);
  }

  private static void addShipmentInfoIn(List<String> primaryHeaderList, List<String> secondaryHeaderList) {
    primaryHeaderList.addAll(Arrays.asList(ExcelHeaderNames.TIPE_PENANGAN, ExcelHeaderNames.KODE_PICKUP_POINT,
        ExcelHeaderNames.PANJANG, ExcelHeaderNames.LEBAR, ExcelHeaderNames.TINGGI, ExcelHeaderNames.BERAT));
    secondaryHeaderList.addAll(Arrays.asList(CnExcelHeaderInfo.TIPE_PENANGAN_IN, CnExcelHeaderInfo.KODE_PICKUP_POINT_IN,
        CnExcelHeaderInfo.PANJANG_IN, CnExcelHeaderInfo.LEBAR_IN, CnExcelHeaderInfo.TINGGI_IN, CnExcelHeaderInfo.BERAT_IN));
  }

  private static void addPriceAndStockIn(List<String> primaryHeaderList, List<String> secondaryHeaderList) {
    primaryHeaderList.addAll(Arrays.asList(ExcelHeaderNames.HARGA, ExcelHeaderNames.HARGA_PENJULAN,
        ExcelHeaderNames.STOK_TERSEDIA, ExcelHeaderNames.STOK_MINIMUM));
    secondaryHeaderList.addAll(Arrays.asList(CnExcelHeaderInfo.HARGA_IN, CnExcelHeaderInfo.HARGA_PENJULAN_IN,
        CnExcelHeaderInfo.STOK_TERSEDIA_IN, CnExcelHeaderInfo.STOK_MINIMUM_IN));
  }

  private static void addSkuVisibilityIn(List<String> primaryHeaderList, List<String> secondaryHeaderList,
      MerchantStatusType merchantStatusType) {
    switch (merchantStatusType){
      case PURE_DELIVERY:{
        primaryHeaderList.add(ExcelHeaderNames.SKU_VISIBILITY_ID);
        secondaryHeaderList.add(CnExcelHeaderInfo.DELIVERY_STATUS_EN);
        break;
      }
      case DELIVERY_AND_CNC:{
        primaryHeaderList.add(ExcelHeaderNames.SKU_VISIBILITY_ID);
        secondaryHeaderList.add(CnExcelHeaderInfo.DELIVERY_STATUS_EN);
        primaryHeaderList.add(ExcelHeaderNames.STATUS_CNC);
        secondaryHeaderList.add(CnExcelHeaderInfo.DELIVERY_STATUS_EN);
        break;
      }
      case BFB:{
        primaryHeaderList.add(ExcelHeaderNames.BFB_BASE_PRICE_ID);
        primaryHeaderList.add(ExcelHeaderNames.BFB_MANAGED_INFO_ID);
        primaryHeaderList.add(ExcelHeaderNames.BFB_STATUS);
        secondaryHeaderList.add(CnExcelHeaderInfo.SECONDARY_HEADER_BFB_BASE_PRICE_IN);
        secondaryHeaderList.add(CnExcelHeaderInfo.SECONDARY_HEADER_BFB_MANAGED_INFO_EN);
        secondaryHeaderList.add(CnExcelHeaderInfo.SECONDARY_HEADER_BFB_STATUS_EN);
        primaryHeaderList.add(ExcelHeaderNames.SKU_VISIBILITY_ID);
        secondaryHeaderList.add(CnExcelHeaderInfo.DELIVERY_STATUS_EN);
        break;
      }
      case BFB_AND_CNC:{
        primaryHeaderList.add(ExcelHeaderNames.BFB_BASE_PRICE_ID);
        primaryHeaderList.add(ExcelHeaderNames.BFB_MANAGED_INFO_ID);
        primaryHeaderList.add(ExcelHeaderNames.BFB_STATUS);
        secondaryHeaderList.add(CnExcelHeaderInfo.SECONDARY_HEADER_BFB_BASE_PRICE_IN);
        secondaryHeaderList.add(CnExcelHeaderInfo.SECONDARY_HEADER_BFB_MANAGED_INFO_EN);
        secondaryHeaderList.add(CnExcelHeaderInfo.SECONDARY_HEADER_BFB_STATUS_EN);
        primaryHeaderList.add(ExcelHeaderNames.SKU_VISIBILITY_ID);
        secondaryHeaderList.add(CnExcelHeaderInfo.DELIVERY_STATUS_EN);
        primaryHeaderList.add(ExcelHeaderNames.STATUS_CNC);
        secondaryHeaderList.add(CnExcelHeaderInfo.DELIVERY_STATUS_EN);
        break;
      }
      default:{
        break;
      }
    }
  }

  public static List<List<String>> cnHeaderListEn(CategoryDetailResponse category,
      MerchantStatusType merchantStatusType, String merchantType, boolean productBundlingEnabled,
      String productBundlingEligibleMerchantTypes, boolean instoreSeller) {
    List<String> primaryHeaderList = new ArrayList<>();
    List<String> secondaryHeaderList = new ArrayList<>();
    primaryHeaderList.addAll(Arrays.asList(ExcelHeaderNames.PRODUCT_NAME, ExcelHeaderNames.DESCRIPTION,
        ExcelHeaderNames.UNIQUE_SELLING_POINT));
    secondaryHeaderList.addAll(Arrays.asList(CnExcelHeaderInfo.PRODUCT_NAME_EN, CnExcelHeaderInfo.DESCRIPTION_EN,
        CnExcelHeaderInfo.UNIQUE_SELLING_POINT_EN));
    primaryHeaderList.add(1, ExcelHeaderNames.MODEL_EAN_UPC);
    primaryHeaderList.add(2, ExcelHeaderNames.SELLER_SKU);
    secondaryHeaderList.add(1, CnExcelHeaderInfo.MODEL_EAN_UPC_EN);
    secondaryHeaderList.add(2, CnExcelHeaderInfo.SELLER_SKU_EN);
    String familyColour = null;
    for (CategoryAttributeResponse categoryAttribute : category.getCategoryAttributes()) {
      if (!categoryAttribute.isMarkForDelete() && (isDescriptiveOrPredefinedAndNotVariantCreation(
          categoryAttribute))) {
        String attributeName = categoryAttribute.getAttribute().getName();
        if (org.apache.commons.lang3.StringUtils.equals(attributeName, Constant.COLOUR_FAMILY)) {
          familyColour = attributeName;
        }
      }
    }
    for (CategoryAttributeResponse categoryAttribute : category.getCategoryAttributes()) {
      if (!categoryAttribute.isMarkForDelete() && isDefiningOrVariantCreation(categoryAttribute)) {
        String attributeName = categoryAttribute.getAttribute().getName();
        if (Objects.nonNull(categoryAttribute.getAttribute().getNameEnglish())) {
          attributeName = categoryAttribute.getAttribute().getNameEnglish();
        }
        if (org.apache.commons.lang3.StringUtils.equals(attributeName, Constant.WARNA)
            || org.apache.commons.lang3.StringUtils.equals(attributeName, Constant.COLOR)) {
          primaryHeaderList.add(attributeName);
          secondaryHeaderList.add(CnExcelHeaderInfo.COLOUR_EN);
          if (StringUtils.isNotEmpty(familyColour)) {
            primaryHeaderList.add(familyColour);
            secondaryHeaderList.add(CnExcelHeaderInfo.COLOUR_FAMILY_EN);
          }
        } else {
          primaryHeaderList.add(attributeName);
          secondaryHeaderList.add(StringUtils.EMPTY);
        }
      }
    }
    primaryHeaderList.add(ExcelHeaderNames.PARENT);
    secondaryHeaderList.add(CnExcelHeaderInfo.PARENT_EN);
    primaryHeaderList.add(BulkCnCreationHeaderNames.VARIANT_IMAGE_EN);
    secondaryHeaderList.add(ExcelHeaderNames.VARIANT_IMAGE_INFO_EN);
    addImagesEn(primaryHeaderList, secondaryHeaderList);
    addShipmentInfoEn(primaryHeaderList, secondaryHeaderList);
    addPriceAndStockEn(primaryHeaderList, secondaryHeaderList);
    addSkuVisibilityEn(primaryHeaderList, secondaryHeaderList, merchantStatusType);
    if (instoreSeller) {
      primaryHeaderList.add(BulkCnCreationHeaderNames.INSTORE);
      secondaryHeaderList.add(BulkCnCreationHeaderNames.INSTORE_DESC_EN);
    }

    for (CategoryAttributeResponse categoryAttribute : category.getCategoryAttributes()) {
      if (!categoryAttribute.isMarkForDelete() && (isDescriptiveOrPredefinedAndNotVariantCreation(categoryAttribute))) {
        String attributeName = categoryAttribute.getAttribute().getName();
        if (Objects.nonNull(categoryAttribute.getAttribute().getNameEnglish())) {
          attributeName = categoryAttribute.getAttribute().getNameEnglish();
        }
        if (Objects.nonNull(attributeName) && (attributeName.contains(Constant.GARANSI)
            || attributeName.contains(Constant.WARRANTY)) && categoryAttribute.getAttribute().isSkuValue()) {
          primaryHeaderList.add(attributeName);
          secondaryHeaderList.add(CnExcelHeaderInfo.RECOMMENDED_ATTRIBUTE_EN);
        } else if (!StringUtils.equalsIgnoreCase(Constant.BRAND, attributeName)
            && !StringUtils.equalsIgnoreCase(Constant.COLOUR_FAMILY, attributeName)
            && categoryAttribute.getAttribute().isBasicView() || categoryAttribute.getAttribute().isSkuValue()) {
          primaryHeaderList.add(attributeName);
          secondaryHeaderList.add(CnExcelHeaderInfo.RECOMMENDED_ATTRIBUTE_EN);
        }
      }
    }

    for (CategoryAttributeResponse categoryAttribute : category.getCategoryAttributes()) {
      if (!categoryAttribute.isMarkForDelete() && (isDescriptiveOrPredefinedAndNotVariantCreation(
          categoryAttribute))) {
        String attributeName = categoryAttribute.getAttribute().getName();
        if (Objects.nonNull(categoryAttribute.getAttribute().getNameEnglish())) {
          attributeName = categoryAttribute.getAttribute().getNameEnglish();
        }
        if (Objects.nonNull(attributeName) && !org.apache.commons.lang3.StringUtils.equals(
            attributeName,
            Constant.COLOUR_FAMILY)) {
          if (org.apache.commons.lang3.StringUtils.equals(categoryAttribute.getAttribute().getName(), Constant.BRAND)) {
            primaryHeaderList.add(Integer.parseInt(Constant.BRAND_INDEX), attributeName);
            secondaryHeaderList.add(Integer.parseInt(Constant.BRAND_INDEX), CnExcelHeaderInfo.BRAND_EN);
          } else if (!attributeName.contains(Constant.GARANSI)
              && !attributeName.contains(Constant.WARRANTY) && !categoryAttribute.getAttribute().isBasicView()
              && !categoryAttribute.getAttribute().isSkuValue()) {
            primaryHeaderList.add(attributeName);
            secondaryHeaderList.add(CnExcelHeaderInfo.OTHER_ATTRIBUTE_EN);
          }
        }
      }
    }
    if (ExcelTemplateUtil.isEligibleForBundleCreation(merchantType, productBundlingEligibleMerchantTypes,
        productBundlingEnabled)) {
      primaryHeaderList.add(BulkCnCreationHeaderNames.CHILD_SKU_EN);
      primaryHeaderList.add(BulkCnCreationHeaderNames.QUANTITY_EN);
      secondaryHeaderList.add(CnExcelHeaderInfo.SECONDARY_HEADER_CHILD_SKU);
      secondaryHeaderList.add(CnExcelHeaderInfo.SECONDARY_HEADER_QUANTITY);
    }
    return Arrays.asList(primaryHeaderList, secondaryHeaderList);
  }

  public static List<List<String>> cnHeaderList(CategoryDetailResponse category, MerchantStatusType merchantStatusType,
      String merchantType, String productBundlingEligibleMerchantTypes, boolean productBundlingEnabled,
      boolean instoreSeller){
    List<String> primaryHeaderList = new ArrayList<>();
    List<String> secondaryHeaderList = new ArrayList<>();
    primaryHeaderList.addAll(Arrays.asList(ExcelHeaderNames.NAMA_PRODUCK, ExcelHeaderNames.DESKRIPSI,
        ExcelHeaderNames.KEUNGGULAN_PRODUK));
    secondaryHeaderList.addAll(Arrays.asList(CnExcelHeaderInfo.NAMA_PRODUCK_IN, CnExcelHeaderInfo.DESKRIPSI_IN,
        CnExcelHeaderInfo.KEUNGGULAN_PRODUK_IN));
    primaryHeaderList.add(1, ExcelHeaderNames.MODEL_EAN_UPC);
    primaryHeaderList.add(2, ExcelHeaderNames.SELLER_SKU);
    secondaryHeaderList.add(1, CnExcelHeaderInfo.MODEL_EAN_UPC_IN);
    secondaryHeaderList.add(2, CnExcelHeaderInfo.SELLER_SKU_IN);
    String familyColour = null;
    for (CategoryAttributeResponse categoryAttribute : category.getCategoryAttributes()) {
      if (!categoryAttribute.isMarkForDelete() && (isDescriptiveOrPredefinedAndNotVariantCreation(
          categoryAttribute))) {
        String attributeName = categoryAttribute.getAttribute().getName();
        if (org.apache.commons.lang3.StringUtils.equals(attributeName, Constant.COLOUR_FAMILY)) {
          familyColour = attributeName;
        }
      }
    }
    for (CategoryAttributeResponse categoryAttribute : category.getCategoryAttributes()) {
      if (!categoryAttribute.isMarkForDelete() && isDefiningOrVariantCreation(categoryAttribute)) {
        String attributeName = categoryAttribute.getAttribute().getName();
        if (org.apache.commons.lang3.StringUtils.equals(attributeName, Constant.WARNA)
            || org.apache.commons.lang3.StringUtils.equals(attributeName, Constant.COLOR)) {
          primaryHeaderList.add(attributeName);
          secondaryHeaderList.add(CnExcelHeaderInfo.WARNA_IN);
          if (StringUtils.isNotEmpty(familyColour)) {
            primaryHeaderList.add(familyColour);
            secondaryHeaderList.add(CnExcelHeaderInfo.COLOUR_FAMILY_IN);
          }
        } else {
          primaryHeaderList.add(attributeName);
          secondaryHeaderList.add(StringUtils.EMPTY);
        }
      }
    }
    primaryHeaderList.add(ExcelHeaderNames.PARENT);
    secondaryHeaderList.add(CnExcelHeaderInfo.PARENT_IN);
    primaryHeaderList.add(BulkCnCreationHeaderNames.VARIANT_IMAGE_ID);
    secondaryHeaderList.add(ExcelHeaderNames.VARIANT_IMAGE_INFO_ID);
    addImagesIn(primaryHeaderList, secondaryHeaderList);
    addShipmentInfoIn(primaryHeaderList, secondaryHeaderList);
    addPriceAndStockIn(primaryHeaderList, secondaryHeaderList);
    addSkuVisibilityIn(primaryHeaderList, secondaryHeaderList, merchantStatusType);
    if (instoreSeller) {
      primaryHeaderList.add(BulkCnCreationHeaderNames.INSTORE);
      secondaryHeaderList.add(BulkCnCreationHeaderNames.INSTORE_DESC_ID);
    }

    for (CategoryAttributeResponse categoryAttribute : category.getCategoryAttributes()) {
      if (!categoryAttribute.isMarkForDelete() && (isDescriptiveOrPredefinedAndNotVariantCreation(categoryAttribute))) {
        String attributeName = categoryAttribute.getAttribute().getName();
        if (Objects.nonNull(attributeName) && (attributeName.contains(Constant.GARANSI)
            || attributeName.contains(Constant.WARRANTY)) && categoryAttribute.getAttribute().isSkuValue()) {
          primaryHeaderList.add(attributeName);
          secondaryHeaderList.add(CnExcelHeaderInfo.RECOMMENDED_ATTRIBUTE_IN);
        } else if (!StringUtils.equalsIgnoreCase(Constant.BRAND, attributeName)
            && !StringUtils.equalsIgnoreCase(Constant.COLOUR_FAMILY, attributeName)
            && categoryAttribute.getAttribute().isBasicView() || categoryAttribute.getAttribute().isSkuValue()) {
          primaryHeaderList.add(attributeName);
          secondaryHeaderList.add(CnExcelHeaderInfo.RECOMMENDED_ATTRIBUTE_IN);
        }
      }
    }

    for (CategoryAttributeResponse categoryAttribute : category.getCategoryAttributes()) {
      if (!categoryAttribute.isMarkForDelete() && (isDescriptiveOrPredefinedAndNotVariantCreation(
          categoryAttribute))) {
        String attributeName = categoryAttribute.getAttribute().getName();
        if (Objects.nonNull(attributeName) && !org.apache.commons.lang3.StringUtils.equals(
            attributeName,
            Constant.COLOUR_FAMILY)) {
          if (org.apache.commons.lang3.StringUtils.equals(categoryAttribute.getAttribute()
              .getName(), Constant.BRAND)) {
            primaryHeaderList.add(Integer.parseInt(Constant.BRAND_INDEX), attributeName);
            secondaryHeaderList.add(Integer.parseInt(Constant.BRAND_INDEX), CnExcelHeaderInfo.BRAND_IN);
          } else if (!attributeName.contains(Constant.GARANSI)
              && !attributeName.contains(Constant.WARRANTY) && !categoryAttribute.getAttribute().isBasicView()
              && !categoryAttribute.getAttribute().isSkuValue()) {
            primaryHeaderList.add(attributeName);
            secondaryHeaderList.add(CnExcelHeaderInfo.OTHER_ATTRIBUTE_IN);
          }
        }
      }
    }
    if (ExcelTemplateUtil.isEligibleForBundleCreation(merchantType, productBundlingEligibleMerchantTypes,
        productBundlingEnabled)) {
      primaryHeaderList.add(BulkCnCreationHeaderNames.CHILD_SKU_ID);
      primaryHeaderList.add(BulkCnCreationHeaderNames.QUANTITY_ID);
      secondaryHeaderList.add(CnExcelHeaderInfo.SECONDARY_HEADER_CHILD_SKU_ID);
      secondaryHeaderList.add(CnExcelHeaderInfo.SECONDARY_HEADER_QUANTITY_ID);
    }
    return Arrays.asList(primaryHeaderList, secondaryHeaderList);
  }
}

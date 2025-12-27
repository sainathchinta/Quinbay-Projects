package com.gdn.x.product.service.impl;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.IntStream;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import com.gdn.x.product.enums.MasterDataAttributeType;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.MasterDataProductAttribute;
import com.gdn.x.product.model.entity.MasterDataProductAttributeValue;
import com.gdn.x.product.model.entity.MasterDataProductImage;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductAttribute;
import com.gdn.x.product.model.entity.ProductAttributeDetail;
import com.gdn.x.product.service.api.ProductService;

@Service
public class DumpServiceImpl {

  private static final String T4 = "\\\\t";
  private static final String T2 = "\\t";
  private static final String N2 = "\\n";
  private static final String N4 = "\\\\n";
  private static final String DELIMITER = "^";
  private static final Logger LOG = LoggerFactory.getLogger("dump.old.prod");
  private static final Logger LOG_ERR = LoggerFactory.getLogger("dump.error");

  @Autowired
  private ProductService productService;

  private void appendSbIfNotOutOfBound(StringBuilder sb, List<ProductAttributeDetail> attr, int i) {
    if (attr.size() > i) {
      sb.append(attr.get(i).getAttributeName());
      sb.append(DELIMITER);
      sb.append(attr.get(i).getAttributeValue());
    } else {
      sb.append(DELIMITER);
    }
  }

  public void dumpAllProductData(String storeId, String locationPath, String fileName) {
    List<String> skus = getListOfOrderFromFile(storeId, locationPath, fileName);
    List<String> failingSku = new ArrayList<>();
    // LOG.info(
    // "GDN Product SKU ID\tNama\tKode Kategori\tBrand\tDescription\tUnique Selling
    // Point\tSpecification Detail\tLength\tWidth\tHeight\tActual Weight\tShipping Weight\tMain
    // Image\tDescriptive Attribute Name\tDescriptive Attribute Value\tDefining Attribute
    // Name\tDefining Attribute Value\tPredefined Attribute Name\tPredefined Attribute Value");
    for (String sku : skus) {
      try {
        Product product = productService.getProductDeletedOrUndeleted(storeId, sku);
        MasterDataProduct masterDataProduct = product.getMasterDataProduct();
        String productName = masterDataProduct.getProductName().trim();
        String categoryCode = product.getMasterCatalog().getCategory().getCategoryCode();
        String brand = masterDataProduct.getBrand();
        String description = Optional.ofNullable(masterDataProduct.getDescription())
            .orElse(StringUtils.EMPTY).replaceAll(N2, N4).replaceAll(T2, T4);
        String uniqueSellingPoint = Optional.ofNullable(masterDataProduct.getUniqueSellingPoint())
            .orElse(StringUtils.EMPTY).replaceAll(N2, N4).replaceAll(T2, T4);
        String specificationDetail = Optional.ofNullable(masterDataProduct.getSpecificationDetail())
            .orElse(StringUtils.EMPTY).replaceAll(N2, N4).replaceAll(T2, T4);
        double Length = masterDataProduct.getLength();
        double width = masterDataProduct.getWidth();
        double height = masterDataProduct.getHeight();
        double weight = masterDataProduct.getWeight();
        double shippingWeight = masterDataProduct.getShippingWeight();
        String mainImage =
            masterDataProduct.getMasterDataProductImages().stream().filter(e -> e.isMainImage())
                .findFirst().orElse(new MasterDataProductImage()).getLocationPath();
        Set<ProductAttributeDetail> definingAttrSet = new HashSet<>();
        for (ProductAttribute productAttribute : product.getDefiningAttributes()) {
          definingAttrSet.addAll(productAttribute.getProductAttributeDetails());
        }
        List<ProductAttributeDetail> definingAttr = new ArrayList<>(definingAttrSet);
        definingAttrSet = null;
        List<ProductAttributeDetail> descriptiveAttr = new ArrayList<>();
        List<ProductAttributeDetail> predefinedAttr = new ArrayList<>();
        for (MasterDataProductAttribute productAttribute : masterDataProduct
            .getMasterDataProductAttributes()) {
          String attributeName = productAttribute.getMasterDataAttribute().getAttributeName();
          if (MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE == productAttribute
              .getMasterDataAttribute().getAttributeType()) {
            for (MasterDataProductAttributeValue attributeValue : productAttribute
                .getMasterDataProductAttributeValues()) {
              descriptiveAttr.add(new ProductAttributeDetail(null, attributeName,
                  attributeValue.getDescriptiveAttributeValue()));
            }
          }
          if (MasterDataAttributeType.PREDEFINED_ATTRIBUTE == productAttribute
              .getMasterDataAttribute().getAttributeType()) {
            for (MasterDataProductAttributeValue attributeValue : productAttribute
                .getMasterDataProductAttributeValues()) {
              predefinedAttr.add(new ProductAttributeDetail(null, attributeName,
                  attributeValue.getPredefinedAllowedAttributeValue().getValue()));
            }
          }
        }

        int maxSize =
            IntStream.of(definingAttr.size(), descriptiveAttr.size(), predefinedAttr.size()).max()
                .getAsInt();
        List<String> combiningAttributes = new ArrayList<>();
        if (maxSize == 0) {
          combiningAttributes.add("^^^^^^");
        } else {
          for (int i = 0; i < maxSize; i++) {
            StringBuilder sb = new StringBuilder();
            appendSbIfNotOutOfBound(sb, descriptiveAttr, i);
            sb.append("^");
            appendSbIfNotOutOfBound(sb, definingAttr, i);
            sb.append("^");
            appendSbIfNotOutOfBound(sb, predefinedAttr, i);
            combiningAttributes.add(sb.toString());
          }
        }
        LOG.info("{}^{}^{}^{}^{}^{}^{}^{}^{}^{}^{}^{}^{}^{}", sku, productName, categoryCode, brand,
            description, uniqueSellingPoint, specificationDetail, Length, width, height, weight,
            shippingWeight, mainImage, combiningAttributes.get(0));
        for (int i = 1; i < combiningAttributes.size(); i++) {
          LOG.info("^^^^^^^^^^^^^{}", combiningAttributes.get(i));
        }
      } catch (Exception e) {
        LOG_ERR.error("dump error on sku = {}", sku, e);
        failingSku.add(sku);
      }
    }
    for (String failing : failingSku) {
      LOG_ERR.error(failing);
    }
  }

  private List<String> getListOfOrderFromFile(String storeId, String locationPath,
      String fileName) {
    String xProductSku = null;
    List<String> xProductSkus = new ArrayList<String>();
    try (FileReader fileReader = new FileReader(locationPath + fileName);
        BufferedReader bufReader = new BufferedReader(fileReader)) {
      while ((xProductSku = bufReader.readLine()) != null) {
        xProductSkus.add(xProductSku);
      }
    } catch (Exception e) {
      LOG.error("#readOrderIdsFromFile error!", e);
    }
    return xProductSkus;
  }

}

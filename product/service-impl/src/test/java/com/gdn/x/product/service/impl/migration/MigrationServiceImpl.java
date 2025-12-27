package com.gdn.x.product.service.impl.migration;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import com.gdn.x.product.dao.api.ItemRepository;
import com.gdn.x.product.dao.api.MasterDataAttributeRepository;
import com.gdn.x.product.dao.api.ProductDb2Repository;
import com.gdn.x.product.dao.api.ProductRepository;
import com.gdn.x.product.dao.api.TicketTemplateRepository;
import com.gdn.x.product.enums.MasterDataAttributeType;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.MasterDataAttribute;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataItemAttributeValue;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.MasterDataProductAttribute;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductAttribute;
import com.gdn.x.product.model.entity.ProductAttributeDetail;
import com.gdn.x.product.model.entity.TicketTemplate;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.product.service.api.ItemCacheableService;
import com.gdn.x.product.service.api.MasterDataService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.api.migration.MigrationService;
import com.google.common.base.Stopwatch;
import com.google.common.collect.Lists;

@Service
public class MigrationServiceImpl implements MigrationService {

  private static final Logger LOGGER = LoggerFactory.getLogger(MigrationServiceImpl.class);

  @Autowired
  @Qualifier("executorService")
  private ExecutorService executorService;

  @Autowired
  private ProductDb2Repository db2Repository;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProductService productService;

  @Autowired
  private ItemRepository itemRepository;

  @Autowired
  private TicketTemplateRepository ticketTemplateRepository;

  @Autowired
  private SystemParameterService systemParameterService;

  @Autowired
  private MasterDataService masterDataService;

  @Autowired
  private ProductHelperService productHelperService;

  @Autowired
  private MasterDataAttributeRepository masterDataAttributeRepository;

  @Autowired
  private ItemCacheableService itemService;

  private void constructMapOfAttributes(String requestId, String productCode,
      Map<String, MasterDataProductAttribute> mapOfDefiningAttrName,
      Map<String, MasterDataAttribute> mapOfDescriptiveAttrName,
      Map<String, MasterDataAttribute> mapOfPredefinedAttrName, Set<String> productCodes)
      throws Exception {
    Map<String, MasterDataProductAndItemsVO> masterDataProductDetail =
        this.masterDataService.getMasterDataProductDetailResponse("10001", "fix-attribute",
            requestId, productCodes, false);
    MasterDataProduct masterDataProduct =
        masterDataProductDetail.get(productCode).getMasterDataProduct();
    for (MasterDataProductAttribute attribute : masterDataProduct.getMasterDataProductAttributes()) {
      MasterDataAttribute masterDataAttribute = attribute.getMasterDataAttribute();
      if (attribute.getMasterDataAttribute().getAttributeType() == MasterDataAttributeType.DEFINING_ATTRIBUTE) {
        attribute.setMasterDataAttribute(this.getOrSaveMasterDataAttribute(masterDataAttribute));
        mapOfDefiningAttrName.put(masterDataAttribute.getAttributeName(), attribute);
      } else if (attribute.getMasterDataAttribute().getAttributeType() == MasterDataAttributeType.PREDEFINED_ATTRIBUTE) {
        mapOfPredefinedAttrName.put(masterDataAttribute.getAttributeName(),
            this.getOrSaveMasterDataAttribute(masterDataAttribute));
      } else {
        mapOfDescriptiveAttrName.put(masterDataAttribute.getAttributeName(),
            this.getOrSaveMasterDataAttribute(masterDataAttribute));
      }
    }
  }

  @Override
  public void fixAttributeData(String requestId, String fileName) {
    String locationPath =
        this.systemParameterService.findValueByStoreIdAndVariable("10001",
            SystemParameterNames.MIGRATION_LOCATION_PATH).getValue();

    BufferedReader br = null;
    Stopwatch stopwatch = Stopwatch.createStarted();
    long elapsed = 0;
    String productCode = "";
    try {
      br = new BufferedReader(new FileReader(locationPath + fileName));
      while ((productCode = br.readLine()) != null) {
        elapsed = stopwatch.elapsed(TimeUnit.NANOSECONDS);
        Set<String> productCodes = new HashSet<String>();
        productCodes.add(productCode);
        List<Product> products = new ArrayList<Product>();
        Map<String, MasterDataProductAttribute> mapOfDefiningAttrName =
            new HashMap<String, MasterDataProductAttribute>();
        Map<String, MasterDataAttribute> mapOfDescriptiveAttrName =
            new HashMap<String, MasterDataAttribute>();
        Map<String, MasterDataAttribute> mapOfPredefinedAttrName =
            new HashMap<String, MasterDataAttribute>();
        try {
          this.constructMapOfAttributes(requestId, productCode, mapOfDefiningAttrName,
              mapOfDescriptiveAttrName, mapOfPredefinedAttrName, productCodes);
          products =
              this.productService.findByStoreIdAndProductCodeInAndMarkForDeleteFalse("10001",
                  productCodes);
        } catch (Exception e) {
          MigrationServiceImpl.LOGGER.error("fixAttributeFailed-onProductCode : {}#{}",
              e.getMessage(), productCode, e);
          continue;
        }

        for (Product product : products) {
          try {
            this.fixDefiningAttributeCode(mapOfDefiningAttrName, product);

            this.fixProductAttributes(mapOfDefiningAttrName, mapOfDescriptiveAttrName,
                mapOfPredefinedAttrName, product);
            this.productRepository.save(product);

            this.fixAttributeOnMasterDataItems(mapOfDefiningAttrName, mapOfDescriptiveAttrName,
                mapOfPredefinedAttrName, product);
            MigrationServiceImpl.LOGGER.error("fixAttribute-succeedOnProductSku#{}",
                product.getProductSku());
          } catch (Exception e) {
            MigrationServiceImpl.LOGGER.error("fixAttribute-failedOnProductSku : {}#{}",
                e.getMessage(), product.getProductSku(), e);
          }
        }
      }
    } catch (Exception e) {
      MigrationServiceImpl.LOGGER.error(e.getMessage(), e);
    } finally {
      if (br != null) {
        try {
          br.close();
        } catch (IOException e) {
          MigrationServiceImpl.LOGGER.error(e.getMessage(), e);
        }
      }
    }
  }

  private void fixAttributeOnMasterDataItems(
      Map<String, MasterDataProductAttribute> mapOfDefiningAttrName,
      Map<String, MasterDataAttribute> mapOfDescriptiveAttrName,
      Map<String, MasterDataAttribute> mapOfPredefinedAttrName, Product product) {
    List<Item> items =
        this.itemService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse("10001", product.getProductSku(), false,
            false, false);
    for (Item item : items) {
      MasterDataItem savedMasterDataItem = item.getMasterDataItem();
      if (savedMasterDataItem != null) {
        try {
          for (MasterDataItemAttributeValue itemAttribute : savedMasterDataItem
              .getMasterDataItemAttributeValues()) {
            MasterDataAttribute masterDataAttribute = itemAttribute.getMasterDataAttribute();
            MasterDataAttribute newMasterDataAttribute =
                this.getAttributeByAttributeName(mapOfDefiningAttrName, mapOfDescriptiveAttrName,
                    mapOfPredefinedAttrName, masterDataAttribute);
            if (newMasterDataAttribute != null) {
              itemAttribute.setMasterDataAttribute(newMasterDataAttribute);
              MigrationServiceImpl.LOGGER.warn("attribute-needTobeDeleted#{}",
                  masterDataAttribute.getId());
            } else {
              MigrationServiceImpl.LOGGER.error("{}#AttributeName  not found: {}#{}",
                  masterDataAttribute.getAttributeType(), masterDataAttribute.getAttributeName(),
                  item.getItemSku());
            }
          }
          this.itemRepository.save(item);
          MigrationServiceImpl.LOGGER.error("fixAttribute-succeedOnItem#{}", item.getItemSku());
        } catch (Exception e) {
          MigrationServiceImpl.LOGGER.error("fixAttribute-failedOnItem : {}#{}", e.getMessage(),
              item.getItemSku(), e);
        }
      }
    }
  }

  private void fixDefiningAttributeCode(
      Map<String, MasterDataProductAttribute> mapOfDefiningAttrName, Product product) {
    for (ProductAttribute definingAttribute : product.getDefiningAttributes()) {
      for (ProductAttributeDetail attrDetail : definingAttribute.getProductAttributeDetails()) {
        MasterDataAttribute masterDataAttribute = null;
        MasterDataProductAttribute masterDataProductAttribute =
            mapOfDefiningAttrName.get(attrDetail.getAttributeName());
        if (masterDataProductAttribute != null) {
          masterDataAttribute = masterDataProductAttribute.getMasterDataAttribute();
        }
        if (masterDataAttribute != null) {
          attrDetail.setAttributeCode(masterDataAttribute.getAttributeCode());
        } else {
          MigrationServiceImpl.LOGGER.error("{}#attributeName : {} not found",
              product.getProductSku(), attrDetail.getAttributeName());
        }
      }
    }
  }

  private void fixProductAttributes(Map<String, MasterDataProductAttribute> mapOfDefiningAttrName,
      Map<String, MasterDataAttribute> mapOfDescriptiveAttrName,
      Map<String, MasterDataAttribute> mapOfPredefinedAttrName, Product product) {
    MasterDataProduct savedMasterDataProduct = product.getMasterDataProduct();
    boolean containsDefining = false;
    if (savedMasterDataProduct != null) {
      for (MasterDataProductAttribute productAttribute : savedMasterDataProduct
          .getMasterDataProductAttributes()) {
        MasterDataAttribute masterDataAttribute = productAttribute.getMasterDataAttribute();
        if (MasterDataAttributeType.DEFINING_ATTRIBUTE == masterDataAttribute.getAttributeType()) {
          MasterDataProductAttribute masterDataProductAttribute =
              mapOfDefiningAttrName.get(masterDataAttribute.getAttributeName());
          if (masterDataProductAttribute != null) {
            productAttribute.setMasterDataProductAttributeValues(masterDataProductAttribute
                .getMasterDataProductAttributeValues());
          }
          containsDefining = true;
        }
        MasterDataAttribute newMasterDataAttribute =
            this.getAttributeByAttributeName(mapOfDefiningAttrName, mapOfDescriptiveAttrName,
                mapOfPredefinedAttrName, masterDataAttribute);
        if (newMasterDataAttribute != null) {
          productAttribute.setMasterDataAttribute(newMasterDataAttribute);
        } else {
          MigrationServiceImpl.LOGGER.error("{}#AttributeName  not found: {}#{}",
              masterDataAttribute.getAttributeType(), masterDataAttribute.getAttributeName(),
              product.getProductSku());
        }
        MigrationServiceImpl.LOGGER.warn("attribute-needTobeDeleted#{}",
            masterDataAttribute.getId());
      }
      if (!containsDefining) {
        savedMasterDataProduct.getMasterDataProductAttributes().addAll(
            mapOfDefiningAttrName.values());
      }
    }
  }

  @Override
  public void generateSpecificationDetail(String storeId, List<String> productSkus) {
    List<List<String>> listProductSkus = Lists.partition(productSkus, 100);
    for (List<String> productSkusToFind : listProductSkus) {
      try {
        List<Product> findByStoreIdAndProductSkuIn =
            this.productService.findByStoreIdAndProductSkuIn(storeId, productSkusToFind);
        for (Product product : findByStoreIdAndProductSkuIn) {
          String generateSpecificationDetail =
              this.productHelperService.generateSpecificationDetail(product);
          product.getMasterDataProduct().setSpecificationDetail(generateSpecificationDetail);
        }
        this.productRepository.saveAll(findByStoreIdAndProductSkuIn);
        MigrationServiceImpl.LOGGER.warn("#success change spec");
      } catch (Exception e) {
        MigrationServiceImpl.LOGGER.warn("failed with error ", e);
        for (String productError : productSkusToFind) {
          MigrationServiceImpl.LOGGER.warn("#errorSku : " + productError);
        }
      }
    }
  }

  private MasterDataAttribute getAttributeByAttributeName(
      Map<String, MasterDataProductAttribute> mapOfDefiningAttrName,
      Map<String, MasterDataAttribute> mapOfDescriptiveAttrName,
      Map<String, MasterDataAttribute> mapOfPredefinedAttrName,
      MasterDataAttribute masterDataAttribute) {
    if (MasterDataAttributeType.DEFINING_ATTRIBUTE == masterDataAttribute.getAttributeType()) {
      MasterDataProductAttribute masterDataProductAttribute =
          mapOfDefiningAttrName.get(masterDataAttribute.getAttributeName());
      if (masterDataProductAttribute != null) {
        return masterDataProductAttribute.getMasterDataAttribute();
      } else {
        return null;
      }
    } else if (MasterDataAttributeType.PREDEFINED_ATTRIBUTE == masterDataAttribute
        .getAttributeType()) {
      return mapOfPredefinedAttrName.get(masterDataAttribute.getAttributeName());
    } else {
      return mapOfDescriptiveAttrName.get(masterDataAttribute.getAttributeName());
    }
  }

  private MasterDataAttribute getOrSaveMasterDataAttribute(MasterDataAttribute masterDataAttribute) {
    MasterDataAttribute attribute =
        this.masterDataAttributeRepository
            .findMasterDataAttributeByAttributeCode(masterDataAttribute.getAttributeCode());
    if (attribute == null) {
      attribute = this.masterDataAttributeRepository.save(masterDataAttribute);
    }
    return attribute;
  }

  @Override
  public void migrateProductData(String fileName, int concurrentNumber) {
    Stopwatch stopwatch = Stopwatch.createStarted();
    long elapsed = 0;
    String locationPath =
        this.systemParameterService.findValueByStoreIdAndVariable("10001",
            SystemParameterNames.MIGRATION_LOCATION_PATH).getValue();

    BufferedReader br = null;
    String productSku = "";
    try {
      br = new BufferedReader(new FileReader(locationPath + fileName));
      int counter = 0;
      while ((productSku = br.readLine()) != null) {
        elapsed = stopwatch.elapsed(TimeUnit.NANOSECONDS);
        this.executorService.submit(new MigrationTask(this.db2Repository, this.productRepository,
            this.itemRepository, productSku));
        MigrationServiceImpl.LOGGER.info("readline : {},{},{}", productSku,
            stopwatch.elapsed(TimeUnit.NANOSECONDS) - elapsed, ++counter);
      }
    } catch (Exception e) {
      MigrationServiceImpl.LOGGER.error(e.getMessage(), e);
    } finally {
      if (br != null) {
        try {
          br.close();
        } catch (IOException e) {
          MigrationServiceImpl.LOGGER.error(e.getMessage(), e);
        }
      }
    }
  }

  @Override
  public void migrateTicketTemplate() throws Exception {
    List<TicketTemplate> ticketTemplates = this.db2Repository.findAllDb2TicketTemplate();
    this.ticketTemplateRepository.saveAll(ticketTemplates);
  }

}

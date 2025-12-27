package com.gdn.x.product.util;


import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.gdn.x.product.enums.DescriptiveAttributeValueType;
import com.gdn.x.product.enums.MasterDataAttributeType;
import com.gdn.x.product.model.entity.MasterDataAttribute;
import com.gdn.x.product.model.entity.MasterDataProductAttribute;
import com.gdn.x.product.model.entity.MasterDataProductAttributeValue;
import com.gdn.x.product.model.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.product.model.entity.ProductAttributeDetail;

public class ProductAttributeUtilTest {

  private List<MasterDataProductAttribute> masterDataAttributeList;
  private static final String DESCRIPTION = "description";

  @BeforeEach
  public void setUp() throws Exception {
    masterDataAttributeList = new ArrayList<>();
    MasterDataProductAttribute masterDataProductAttribute = new MasterDataProductAttribute();
    MasterDataAttribute masterDataAttribute = new MasterDataAttribute();
    masterDataAttribute.setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
    masterDataAttribute.setSkuValue(false);
    masterDataAttribute.setMandatory(false);
    MasterDataProductAttributeValue masterDataProductAttributeValue = new MasterDataProductAttributeValue();
    masterDataProductAttributeValue.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
    masterDataProductAttributeValue.setDescriptiveAttributeValue(DESCRIPTION);
    MasterDataAttribute masterDataAttribute1 = new MasterDataAttribute();
    masterDataAttribute1.setAttributeType(MasterDataAttributeType.PREDEFINED_ATTRIBUTE);
    masterDataAttribute1.setSkuValue(false);
    masterDataAttribute1.setMandatory(false);
    MasterDataProductAttribute masterDataProductAttribute1 = new MasterDataProductAttribute();
    masterDataProductAttribute1.setMasterDataAttribute(masterDataAttribute1);
    MasterDataProductAttributeValue masterDataProductAttributeValue1 = new MasterDataProductAttributeValue();
    masterDataProductAttributeValue1.setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValue());
    masterDataProductAttributeValue1.setMarkForDelete(false);
    masterDataProductAttribute.setMasterDataProductAttributeValues(Arrays.asList(masterDataProductAttributeValue));
    masterDataProductAttribute1.setMasterDataProductAttributeValues(Arrays.asList(masterDataProductAttributeValue1));
    masterDataProductAttribute.setMasterDataAttribute(masterDataAttribute);
    masterDataAttributeList.add(masterDataProductAttribute);
    masterDataAttributeList.add(masterDataProductAttribute1);
  }

  @Test
  public void fetchDescriptiveAttributeFromMasterDataProduct() {
    List<ProductAttributeDetail> productAttributeDetails =
        ProductAttributeUtil.fetchDescriptiveAttributeFromMasterDataProduct(masterDataAttributeList);
    Assertions.assertNotNull(productAttributeDetails);
    Assertions.assertEquals(2, productAttributeDetails.size());
  }
}
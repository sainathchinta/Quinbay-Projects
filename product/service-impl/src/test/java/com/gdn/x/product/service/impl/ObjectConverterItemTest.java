package com.gdn.x.product.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataItemAttributeValue;
import com.gdn.x.product.model.entity.MasterDataItemImage;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;

public class ObjectConverterItemTest {

  private static final String PRODUCT_CODE = "productCode";

  private static final String DESCRIPTION = "DESCRIPTION";

  private static final String GENERATED_ITEM_NAME = "item-name";

  private static final double ITEM_LENGTH = 50;

  private static final double ITEM_WIDTH = 30;

  private static final double ITEM_HEIGHT = 165;

  private static final boolean IS_ACTIVATED = false;

  private static final Double ITEM_DELIVERY_WEIGHT = 1.0;

  private static final Double ITEM_WEIGHT = 1.0;

  private static final String SKU_CODE = "sku-code";

  private static final String UPC_CODE = "upc-code";

  private static final boolean IS_VIEWABLE = true;

  private static final String HASH = "hash";

  private static final String ATTRIBUTE_CODE = "attribute-code";

  private static final String ATTRIBUTE_VALUE = "attribute-value";

  private static final String LOCATION_PATH = "location-path";

  private static final boolean IS_MAIN_IMAGE = true;

  private static final Integer SEQUENCE = 1;

  @InjectMocks
  private ObjectConverterServiceImpl objectConverterServiceImpl;

  private ProductItemDetailResponse productItemDetailResponse;

  private AttributeResponse attributeResponse;

  private ArrayList<ProductItemAttributeValueResponse> productItemAttributeValueResponses;

  private ProductItemAttributeValueResponse productItemAttributeValueResponse;

  private Image image;

  private ArrayList<Image> images;

  @Test
  public void convertToMasterDataItemTest() {
    MasterDataItem result =
        this.objectConverterServiceImpl.convertToMasterDataItem(this.productItemDetailResponse);

    assertEquals(result.getGeneratedItemName(),
        this.productItemDetailResponse.getGeneratedItemName());
    assertEquals(result.getItemLength(), this.productItemDetailResponse.getItemLength());
    assertEquals(result.getItemWidth(), this.productItemDetailResponse.getItemWidth());
    assertEquals(result.getItemHeight(), this.productItemDetailResponse.getItemHeight());
    assertEquals(result.isActivated(), this.productItemDetailResponse.isActivated());
    assertEquals(result.getHash(), new String(this.productItemDetailResponse.getHash()));
    assertEquals(result.getItemDeliveryWeight(), this.productItemDetailResponse.getItemDeliveryWeight());
    assertEquals(result.getItemWeight(), this.productItemDetailResponse.getItemWeight());
    assertEquals(result.getSkuCode(), this.productItemDetailResponse.getSkuCode());
    assertEquals(result.getUpcCode(), this.productItemDetailResponse.getUpcCode());
    assertEquals(result.isViewable(), this.productItemDetailResponse.isViewable());

    assertEquals(result.getMasterDataItemAttributeValues().size(), 1);
    List<MasterDataItemAttributeValue> itemAttributeValues =
        result.getMasterDataItemAttributeValues();
    for (int i = 0; i < itemAttributeValues.size(); i++) {
      assertEquals(itemAttributeValues.get(i).getAttributeValue(),
          this.productItemAttributeValueResponses.get(i).getValue());
      // assertEquals(itemAttributeValues.get(0).getAttributeCode(),
      // this.productItemAttributeValueResponses.get(i).getAttributeResponse().getAttributeCode());
    }

    assertEquals(result.getMasterDataItemImages().size(), 1);
    List<MasterDataItemImage> itemImages = result.getMasterDataItemImages();
    for (int i = 0; i < itemImages.size(); i++) {
      assertEquals(itemImages.get(i).getLocationPath(), this.images.get(i).getLocationPath());
      assertEquals(itemImages.get(i).getSequence(), this.images.get(i).getSequence());
    }
  }

  @Test
  public void convertToMasterDataItemTestWithNullProductItemDetailResponse() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.objectConverterServiceImpl.convertToMasterDataItem(null));
  }

  @BeforeEach
  public void setUp() {
    openMocks(this);
    ProductResponse productResponse = new ProductResponse();
    productResponse.setProductCode(ObjectConverterItemTest.PRODUCT_CODE);

    this.productItemDetailResponse = new ProductItemDetailResponse();
    this.productItemDetailResponse
        .setGeneratedItemName(ObjectConverterItemTest.GENERATED_ITEM_NAME);
    this.productItemDetailResponse.setItemLength(ObjectConverterItemTest.ITEM_LENGTH);
    this.productItemDetailResponse.setItemWidth(ObjectConverterItemTest.ITEM_WIDTH);
    this.productItemDetailResponse.setItemHeight(ObjectConverterItemTest.ITEM_HEIGHT);
    this.productItemDetailResponse.setActivated(ObjectConverterItemTest.IS_ACTIVATED);
    this.productItemDetailResponse.setHash(ObjectConverterItemTest.HASH.getBytes());
    this.productItemDetailResponse
        .setItemDeliveryWeight(ObjectConverterItemTest.ITEM_DELIVERY_WEIGHT);
    this.productItemDetailResponse.setItemWeight(ObjectConverterItemTest.ITEM_WEIGHT);
    this.productItemDetailResponse.setSkuCode(ObjectConverterItemTest.SKU_CODE);
    this.productItemDetailResponse.setUpcCode(ObjectConverterItemTest.UPC_CODE);
    this.productItemDetailResponse.setViewable(ObjectConverterItemTest.IS_VIEWABLE);
    this.productItemDetailResponse.setProductResponse(productResponse);
    this.productItemDetailResponse.setDangerousGoodsLevel(0);
    this.productItemAttributeValueResponses = new ArrayList<ProductItemAttributeValueResponse>();

    this.productItemAttributeValueResponse = new ProductItemAttributeValueResponse();
    this.productItemAttributeValueResponse.setValue(ObjectConverterItemTest.ATTRIBUTE_VALUE);

    this.attributeResponse = new AttributeResponse();
    this.attributeResponse.setAttributeCode(ObjectConverterItemTest.ATTRIBUTE_CODE);
    this.attributeResponse.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.toString());
    this.attributeResponse.setDescription(ObjectConverterItemTest.DESCRIPTION.getBytes());

    this.productItemAttributeValueResponse.setAttributeResponse(this.attributeResponse);

    this.productItemAttributeValueResponses.add(this.productItemAttributeValueResponse);

    this.productItemDetailResponse
        .setProductItemAttributeValueResponses(this.productItemAttributeValueResponses);

    this.images = new ArrayList<Image>();
    this.image = new Image();
    this.image.setLocationPath(ObjectConverterItemTest.LOCATION_PATH);
    this.image.setMainImages(ObjectConverterItemTest.IS_MAIN_IMAGE);
    this.image.setSequence(ObjectConverterItemTest.SEQUENCE);
    this.images.add(this.image);

    this.productItemDetailResponse.setImages(this.images);
  }

  @AfterEach
  public void tearDown() {

  }
}

package com.gdn.x.product.service.impl;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;

import com.gdn.x.product.dao.api.MasterDataAttributeRepository;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.MasterDataAttribute;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataItemAttributeValue;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.MasterDataProductAttribute;
import com.gdn.x.product.model.entity.Product;

public class MasterDataAttributeServiceImplTest {

  private static final MasterDataAttribute MASTER_DATA_ATTRIBUTE = new MasterDataAttribute();

  private static final String ATTRCODE_NOT_FOUND = "attrcode-2";
  private static final String ATTRCODE_FOUND = "attrcode-1";

  @InjectMocks
  private MasterDataAttributeServiceImpl masterDataAttributeServiceImpl;

  @Mock
  private MasterDataAttributeRepository masterDataAttributeRepository;

  private Product product;

  private Item item;

  private MasterDataAttribute masterDataAttrFound;

  private MasterDataAttribute masterDataAttrNotFound;

  private List<Item> items;

  @BeforeEach
  public void init() {
    openMocks(this);
    this.masterDataAttrFound = new MasterDataAttribute();
    this.masterDataAttrFound.setAttributeCode(MasterDataAttributeServiceImplTest.ATTRCODE_FOUND);
    this.masterDataAttrNotFound = new MasterDataAttribute();
    this.masterDataAttrNotFound
        .setAttributeCode(MasterDataAttributeServiceImplTest.ATTRCODE_NOT_FOUND);

    this.item = new Item();
    this.item.setMasterDataItem(new MasterDataItem());
    MasterDataItemAttributeValue itemAttribute1 = new MasterDataItemAttributeValue();
    itemAttribute1.setMasterDataAttribute(this.masterDataAttrFound);
    MasterDataItemAttributeValue itemAttribute2 = new MasterDataItemAttributeValue();
    itemAttribute2.setMasterDataAttribute(this.masterDataAttrNotFound);

    List<MasterDataItemAttributeValue> masterDataItemAttributes =
        Arrays.asList(itemAttribute1, itemAttribute2);
    this.item.getMasterDataItem().setMasterDataItemAttributeValues(masterDataItemAttributes);
    this.items = Arrays.asList(this.item);

    this.product = new Product();
    this.product.setMasterDataProduct(new MasterDataProduct());
    MasterDataProductAttribute masterDataProductAttribute1 = new MasterDataProductAttribute();
    masterDataProductAttribute1.setMasterDataAttribute(this.masterDataAttrFound);
    MasterDataProductAttribute masterDataProductAttribute2 = new MasterDataProductAttribute();
    masterDataProductAttribute2.setMasterDataAttribute(this.masterDataAttrNotFound);
    List<MasterDataProductAttribute> masterDataProductAttributes =
        Arrays.asList(masterDataProductAttribute1, masterDataProductAttribute2);
    this.product.getMasterDataProduct().setMasterDataProductAttributes(masterDataProductAttributes);
    when(
        this.masterDataAttributeRepository
            .findMasterDataAttributeByAttributeCode(MasterDataAttributeServiceImplTest.ATTRCODE_FOUND))
        .thenReturn(MasterDataAttributeServiceImplTest.MASTER_DATA_ATTRIBUTE);
    when(
        this.masterDataAttributeRepository
            .findMasterDataAttributeByAttributeCode(MasterDataAttributeServiceImplTest.ATTRCODE_NOT_FOUND))
        .thenReturn(null);
  }

  @Test
  public void setAndSaveMasterDataAttribute() {
    this.masterDataAttributeServiceImpl.setAndSaveMasterDataAttributeProduct(this.product);
    verify(this.masterDataAttributeRepository, times(1)).findMasterDataAttributeByAttributeCode(
        MasterDataAttributeServiceImplTest.ATTRCODE_FOUND);
    verify(this.masterDataAttributeRepository, times(1)).findMasterDataAttributeByAttributeCode(
        MasterDataAttributeServiceImplTest.ATTRCODE_NOT_FOUND);
    verify(this.masterDataAttributeRepository, times(1)).save(this.masterDataAttrNotFound);
  }

  @Test
  public void setAndSaveMasterDataAttribute_emptyAttributes() {
    this.product.getMasterDataProduct().setMasterDataProductAttributes(new ArrayList<>());
    this.masterDataAttributeServiceImpl.setAndSaveMasterDataAttributeProduct(this.product);
  }

  @Test
  public void getOrSaveNewMasterDataAttributeTest() {
    when(this.masterDataAttributeRepository
        .findMasterDataAttributeByAttributeCode(MasterDataAttributeServiceImplTest.ATTRCODE_FOUND))
        .thenReturn(MasterDataAttributeServiceImplTest.MASTER_DATA_ATTRIBUTE);
    masterDataAttributeServiceImpl.getOrSaveNewMasterDataAttribute(masterDataAttrFound);
    verify(masterDataAttributeRepository)
        .findMasterDataAttributeByAttributeCode(MasterDataAttributeServiceImplTest.ATTRCODE_FOUND);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.masterDataAttributeRepository);
  }
}

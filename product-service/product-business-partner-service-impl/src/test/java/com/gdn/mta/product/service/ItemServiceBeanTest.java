package com.gdn.mta.product.service;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.domain.event.modal.ItemStatusDomainEvent;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.enums.ProductStatus;
import com.gdn.mta.product.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import org.slf4j.MDC;

public class ItemServiceBeanTest {

  private static final String STORE_ID = "storeId";
  private static final String PRODUCT_CODE = "productCode";
  private static final String PRODUCT_ID = "productId";
  private static final String ITEM_SKU = "itemSku";

  @InjectMocks
  private ItemServiceBean itemServiceBean;

  @Mock
  private ItemStatusPublisherService itemStatusPublisherService;

  @Mock
  private ProductCollectionRepository productCollectionRepository;

  @Mock
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  private ProductCollection productCollection;
  private List<ProductBusinessPartner> productBusinessPartners;
  private ProductBusinessPartner productBusinessPartner;
  private ProductItemBusinessPartner productItemBusinessPartner;
  private ItemStatusDomainEvent itemStatusDomainEvent;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    MDC.put("storeId", STORE_ID);
    productCollection = new ProductCollection();
    productCollection.setProductId(PRODUCT_ID);
    productBusinessPartners = new ArrayList<>();
    productBusinessPartner = new ProductBusinessPartner();
    productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(ITEM_SKU);
    productBusinessPartner
        .setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner));
    productBusinessPartners.add(productBusinessPartner);
    itemStatusDomainEvent = new ItemStatusDomainEvent(ProductStatus.ACTIVE,
        new HashSet<>(Arrays.asList(ITEM_SKU)), STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    when(productBusinessPartnerRepository.findByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(productBusinessPartners);
    when(itemStatusPublisherService.publishItemStatusDomainEvent(itemStatusDomainEvent))
        .thenReturn(new ItemStatusDomainEvent());
  }

  @Test
  public void publishActivatedItemStatusEvent() throws Exception {
    itemServiceBean.publishItemStatusEvent(PRODUCT_CODE, ProductStatus.ACTIVE);
    verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(productBusinessPartnerRepository).findByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(itemStatusPublisherService).publishItemStatusDomainEvent(itemStatusDomainEvent);

  }

  @Test
  public void publishActivatedItemStatusEvent_emptyItemSkuList() throws Exception {
    when(productBusinessPartnerRepository.findByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(Arrays.asList(new ProductBusinessPartner()));
    itemServiceBean.publishItemStatusEvent(PRODUCT_CODE, ProductStatus.ACTIVE);
    verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(productBusinessPartnerRepository).findByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
  }

  @Test
  public void publishActivatedItemStatusEvent_nullProductCollection() throws Exception {
    when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(null);
    itemServiceBean.publishItemStatusEvent(PRODUCT_CODE, ProductStatus.ACTIVE);
    verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void publishActivatedItemStatusEvent_emptyList() throws Exception {
    when(productBusinessPartnerRepository.findByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(new ArrayList<>());
    itemServiceBean.publishItemStatusEvent(PRODUCT_CODE, ProductStatus.ACTIVE);
    verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(productBusinessPartnerRepository).findByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    verifyNoMoreInteractions(this.itemStatusPublisherService, this.productCollectionRepository,
        this.productBusinessPartnerRepository);
  }
}

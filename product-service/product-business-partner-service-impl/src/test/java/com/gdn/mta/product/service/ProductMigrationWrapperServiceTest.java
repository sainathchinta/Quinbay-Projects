package com.gdn.mta.product.service;

import java.util.ArrayList;
import java.util.Arrays;

import com.gda.mta.product.dto.ProductAndL5MigrationRequest;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.enums.ProductType;
import com.gdn.partners.product.orchestrator.constant.ProductLevel1State;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.data.domain.PageImpl;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.entity.ProductMigration;
import com.gdn.mta.product.enums.MigrationStatus;
import com.gdn.mta.product.repository.ProductLevel3Repository;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;

public class ProductMigrationWrapperServiceTest {

  private static final String PRODUCT_CODE = "productCode";
  private static final String PRODUCT_ID = "productId";
  private static final String PRODUCT_SKU = "productSku";
  private static final String PRODUCT_SKU1 = "productSku1";
  private static final String BUSINESS_PARTNER_ID = "merchantCode";
  private static final String BUSINESS_PARTNER_ID1 = "merchantCode1";
  private static final String MERCHANT_TYPE = "CC";
  private static final String MERCHANT_STATUS = "ACTIVE";
  private static final String BUSINESS_PARTNER_NAME = "businessPartnerName";
  private static final String REQUEST_ID = "requestId";
  private static final String ATTRIBUTE_CODE = "ATTRIBUTE_CODE";
  private static final String ATTRIBUTE_NAME = "ATTRIBUTE_NAME";
  private static final String ATTRIBUTE_VALUE = "ATTRIBUTE_VALUE";

  private ProductMigration productMigration;
  private ProductMigration productMigration1;
  private BusinessPartnerResponse businessPartnerResponse;
  private BusinessPartnerResponse businessPartnerResponse1;

  @InjectMocks
  private ProductMigrationWrapperServiceImpl productMigrationWrapperService;

  @Mock
  private ProductMigrationService productMigrationService;

  @Mock
  private XProductOutbound xProductOutbound;

  @Mock
  private ProductOutbound productOutbound;

  @Mock
  private ProductService productService;

  @Captor
  private ArgumentCaptor<ProductMigration> productMigrationArgumentCaptor;

  @Mock
  private ProductLevel3Repository productLevel3Repository;

  @Mock
  private ProductBusinessPartnerService productBusinessPartnerService;


  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);

    productMigration = new ProductMigration();
    productMigration.setGdnProductSku(PRODUCT_SKU);
    productMigration.setMigratedProductCode(PRODUCT_CODE);
    productMigration.setBusinessPartnerId(BUSINESS_PARTNER_ID);
    productMigration.setProductCode(PRODUCT_CODE);

    productMigration1 = new ProductMigration();
    productMigration1.setGdnProductSku(PRODUCT_SKU1);
    productMigration1.setMigratedProductCode(PRODUCT_CODE);
    productMigration1.setBusinessPartnerId(BUSINESS_PARTNER_ID);
    productMigration1.setProductCode(PRODUCT_CODE);

    businessPartnerResponse = new BusinessPartnerResponse();
    businessPartnerResponse.setBusinessPartnerCode(BUSINESS_PARTNER_ID);
    businessPartnerResponse.setMerchantType(MERCHANT_TYPE);
    businessPartnerResponse.setMerchantStatus(MERCHANT_STATUS);
    businessPartnerResponse.setBusinessPartnerName(BUSINESS_PARTNER_NAME);

    businessPartnerResponse1 = new BusinessPartnerResponse();
    businessPartnerResponse1.setBusinessPartnerCode(BUSINESS_PARTNER_ID1);
    businessPartnerResponse1.setMerchantType(Constants.CM_MERCHANT);
    businessPartnerResponse1.setMerchantStatus(MERCHANT_STATUS);
    businessPartnerResponse1.setBusinessPartnerName(BUSINESS_PARTNER_NAME);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productOutbound);
    Mockito.verifyNoMoreInteractions(xProductOutbound);
    Mockito.verifyNoMoreInteractions(productService);
    Mockito.verifyNoMoreInteractions(productMigrationService, productBusinessPartnerService);
  }

  @Test
  public void updateMigratedProductCodeTest() throws Exception {
    Mockito.when(productMigrationService.getProductMigrationByProductSkus(Arrays.asList(PRODUCT_SKU)))
        .thenReturn(Arrays.asList(productMigration));
    Mockito.doNothing().when(xProductOutbound).updateMigratedProductCode(PRODUCT_SKU, PRODUCT_CODE, true);
    productMigrationWrapperService.updateMigratedProductCode(Arrays.asList(PRODUCT_SKU));
    Mockito.verify(productMigrationService).getProductMigrationByProductSkus(Arrays.asList(PRODUCT_SKU));
    Mockito.verify(xProductOutbound).updateMigratedProductCode(PRODUCT_SKU, PRODUCT_CODE, true);
  }

  @Test
  public void retryFailedMigratedProductsNoMigrationTest() throws Exception {
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    ProductResponse productResponse = new ProductResponse();
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setDescription(new String());
    masterDataProductDTO.setLongDescription(new String());
    productAndItemsResponse.setProduct(productResponse);
    productResponse.setMasterDataProduct(masterDataProductDTO);
    productResponse.setSuspended(false);
    productResponse.setMarkForDelete(true);
    productResponse.setSynchronized(true);
    productMigration.setRetryCount(0);
    productMigration.setSyncStatus(true);
    productMigration.setProductCode(PRODUCT_CODE);
    productMigration.setMerchantType(Constants.CM_MERCHANT);
    productMigration.setMigratedProductCode(null);
    Mockito.when(productMigrationService.findFailedProductMigration(MigrationStatus.FAILED.getMigrationStatus(), 1, 3))
        .thenReturn(new PageImpl<>(Arrays.asList(productMigration)));
    Mockito.when(xProductOutbound.getProductAndItemsWithProductData(true, PRODUCT_SKU, true, false))
        .thenReturn(new GdnRestSingleResponse<>(productAndItemsResponse, REQUEST_ID));
    Mockito.when(productMigrationService.save(productMigrationArgumentCaptor.capture())).thenReturn(productMigration);
    productMigrationWrapperService.retryFailedMigratedProducts(1, 3);
    Mockito.verify(productMigrationService)
        .findFailedProductMigration(MigrationStatus.FAILED.getMigrationStatus(), 1, 3);
    Mockito.verify(xProductOutbound).getProductAndItemsWithProductData(true, PRODUCT_SKU, true, false);
    Mockito.verify(productMigrationService).save(productMigrationArgumentCaptor.getAllValues().get(0));
  }


  @Test
  public void retryFailedMigratedProductsNullProductCodeSyncTest() throws Exception {
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    ProductResponse productResponse = new ProductResponse();
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setDescription(new String());
    masterDataProductDTO.setLongDescription(new String());
    productAndItemsResponse.setProduct(productResponse);
    productResponse.setMasterDataProduct(masterDataProductDTO);
    productResponse.setSuspended(true);
    productResponse.setSynchronized(true);
    productMigration.setRetryCount(0);
    productMigration.setSyncStatus(true);
    productMigration.setMigratedProductCode(null);
    productMigration.setMerchantType(Constants.CM_MERCHANT);
    Mockito.when(productMigrationService.findFailedProductMigration(MigrationStatus.FAILED.getMigrationStatus(), 1, 3))
        .thenReturn(new PageImpl<>(Arrays.asList(productMigration)));
    Mockito.when(productService.generateProductCode()).thenReturn(PRODUCT_CODE);
    Mockito.when(productOutbound.migrateProduct(PRODUCT_CODE, PRODUCT_CODE, BUSINESS_PARTNER_ID, null))
        .thenReturn(PRODUCT_ID);
    Mockito.when(xProductOutbound.getProductAndItemsWithProductData(true, PRODUCT_SKU, true, false))
        .thenReturn(new GdnRestSingleResponse<>(productAndItemsResponse, REQUEST_ID));
    Mockito.doNothing().when(productService)
        .updateProductCollectionForMigratedProduct(productMigrationArgumentCaptor.capture(), Mockito.any(ProductAndItemsResponse.class));
    Mockito.doNothing().when(xProductOutbound).updateMigratedProductCode(PRODUCT_SKU, PRODUCT_CODE, false);
    Mockito.when(productOutbound.republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE))).thenReturn(true);
    Mockito.doNothing().when(productService).saveMasterProductMigrationHistory(productMigrationArgumentCaptor.capture());
    Mockito.when(productMigrationService.save(productMigrationArgumentCaptor.capture())).thenReturn(productMigration);
    productMigrationWrapperService.retryFailedMigratedProducts(1, 3);
    Mockito.verify(productMigrationService)
        .findFailedProductMigration(MigrationStatus.FAILED.getMigrationStatus(), 1, 3);
    Mockito.verify(xProductOutbound).getProductAndItemsWithProductData(true, PRODUCT_SKU, true, false);
    Mockito.verify(productOutbound)
        .migrateProduct(Mockito.any(), Mockito.eq(PRODUCT_CODE), Mockito.eq(BUSINESS_PARTNER_ID), Mockito.any());
    Mockito.verify(productService)
        .updateProductCollectionForMigratedProduct(Mockito.eq(productMigrationArgumentCaptor.getAllValues().get(0)), Mockito.any(ProductAndItemsResponse.class));
    Mockito.verify(xProductOutbound).updateMigratedProductCode(PRODUCT_SKU, PRODUCT_CODE, false);
    Mockito.verify(productOutbound).republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE));
    Mockito.verify(productService).saveMasterProductMigrationHistory(productMigrationArgumentCaptor.getAllValues().get(1));
    Mockito.verify(productMigrationService).save(productMigrationArgumentCaptor.getAllValues().get(2));
    Mockito.verify(productService).generateProductCode();
    Assertions.assertTrue(productMigrationArgumentCaptor.getAllValues().get(2).isL3Updated());
    Assertions.assertTrue(productMigrationArgumentCaptor.getAllValues().get(2).isProductPublished());
    Assertions.assertEquals(1, productMigrationArgumentCaptor.getAllValues().get(2).getRetryCount());
  }

  @Test
  public void retryFailedMigratedProductsNullProductCodeTest() throws Exception {
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    ProductResponse productResponse = new ProductResponse();
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setDescription(new String());
    masterDataProductDTO.setLongDescription(new String());
    productAndItemsResponse.setProduct(productResponse);
    productResponse.setMasterDataProduct(masterDataProductDTO);
    productResponse.setSuspended(true);
    productMigration.setRetryCount(0);
    productMigration.setSyncStatus(true);
    productMigration.setProductCode(PRODUCT_CODE);
    productMigration.setMigratedProductCode(null);
    productMigration.setMerchantType(Constants.CM_MERCHANT);
    Mockito.when(productMigrationService.findFailedProductMigration(MigrationStatus.FAILED.getMigrationStatus(), 1, 3))
        .thenReturn(new PageImpl<>(Arrays.asList(productMigration)));
    Mockito.when(productService.generateProductCode()).thenReturn(PRODUCT_CODE);
    Mockito.when(productOutbound.migrateProduct(PRODUCT_CODE, PRODUCT_CODE, BUSINESS_PARTNER_ID, null))
        .thenReturn(PRODUCT_ID);
    Mockito.when(xProductOutbound.getProductAndItemsWithProductData(true, PRODUCT_SKU, true, false))
        .thenReturn(new GdnRestSingleResponse<>(productAndItemsResponse, REQUEST_ID));
    Mockito.doNothing().when(productService)
        .updateProductCollectionForMigratedProduct(productMigrationArgumentCaptor.capture(), Mockito.any(ProductAndItemsResponse.class));
    Mockito.doNothing().when(xProductOutbound).updateMigratedProductCode(PRODUCT_SKU, PRODUCT_CODE, false);
    Mockito.when(productOutbound.republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE))).thenReturn(true);
    Mockito.doNothing().when(productService).saveMasterProductMigrationHistory(productMigrationArgumentCaptor.capture());
    Mockito.when(productMigrationService.save(productMigrationArgumentCaptor.capture())).thenReturn(productMigration);
    productMigrationWrapperService.retryFailedMigratedProducts(1, 3);
    Mockito.verify(productMigrationService)
        .findFailedProductMigration(MigrationStatus.FAILED.getMigrationStatus(), 1, 3);
    Mockito.verify(xProductOutbound).getProductAndItemsWithProductData(true, PRODUCT_SKU, true, false);
    Mockito.verify(productOutbound)
        .migrateProduct((String) Mockito.any(), Mockito.eq(PRODUCT_CODE), Mockito.eq(BUSINESS_PARTNER_ID),
            Mockito.any(ProductRequest.class));
    Mockito.verify(productService)
        .updateProductCollectionForMigratedProduct(Mockito.eq(productMigrationArgumentCaptor.getAllValues().get(0)), Mockito.any(ProductAndItemsResponse.class));
    Mockito.verify(xProductOutbound).updateMigratedProductCode(PRODUCT_SKU, PRODUCT_CODE, false);
    Mockito.verify(productOutbound).republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE));
    Mockito.verify(productService).saveMasterProductMigrationHistory(productMigrationArgumentCaptor.getAllValues().get(1));
    Mockito.verify(productMigrationService).save(productMigrationArgumentCaptor.getAllValues().get(2));
    Mockito.verify(productService).generateProductCode();
    Assertions.assertTrue(productMigrationArgumentCaptor.getAllValues().get(2).isL3Updated());
    Assertions.assertTrue(productMigrationArgumentCaptor.getAllValues().get(2).isProductPublished());
    Assertions.assertEquals(1, productMigrationArgumentCaptor.getAllValues().get(2).getRetryCount());
  }

  @Test
  public void retryFailedMigratedProductsPCBUpdateFailedSyncTest() throws Exception {
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    ProductResponse productResponse = new ProductResponse();
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setDescription(new String());
    masterDataProductDTO.setLongDescription(new String());
    productAndItemsResponse.setProduct(productResponse);
    productResponse.setMasterDataProduct(masterDataProductDTO);
    productMigration.setRetryCount(0);
    productMigration.setSyncStatus(true);
    productMigration.setProductCode(PRODUCT_CODE);
    productMigration.setMerchantType(Constants.CM_MERCHANT);
    Mockito.when(productMigrationService.findFailedProductMigration(MigrationStatus.FAILED.getMigrationStatus(), 1, 3))
        .thenReturn(new PageImpl<>(Arrays.asList(productMigration)));
    Mockito.when(productOutbound.migrateProduct(PRODUCT_CODE, PRODUCT_CODE, BUSINESS_PARTNER_ID, null))
        .thenReturn(PRODUCT_ID);
    Mockito.when(xProductOutbound.getProductAndItemsWithProductData(true, PRODUCT_SKU, true, false))
        .thenReturn(new GdnRestSingleResponse<>(null, REQUEST_ID));
    Mockito.doNothing().when(productService)
        .updateProductCollectionForMigratedProduct(productMigrationArgumentCaptor.capture(), Mockito.any());
    Mockito.doNothing().when(xProductOutbound).updateMigratedProductCode(PRODUCT_SKU, PRODUCT_CODE, false);
    Mockito.when(productOutbound.republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE))).thenReturn(true);
    Mockito.doNothing().when(productService).saveMasterProductMigrationHistory(productMigrationArgumentCaptor.capture());
    Mockito.when(productMigrationService.save(productMigrationArgumentCaptor.capture())).thenReturn(productMigration);
    productMigrationWrapperService.retryFailedMigratedProducts(1, 3);
    Mockito.verify(productMigrationService)
        .findFailedProductMigration(MigrationStatus.FAILED.getMigrationStatus(), 1, 3);
    Mockito.verify(xProductOutbound).getProductAndItemsWithProductData(true, PRODUCT_SKU, true, false);
    Mockito.verify(productOutbound).migrateProduct(PRODUCT_CODE, PRODUCT_CODE, BUSINESS_PARTNER_ID, null);
    Mockito.verify(productService)
        .updateProductCollectionForMigratedProduct(productMigrationArgumentCaptor.getAllValues().get(0), null);
    Mockito.verify(xProductOutbound).updateMigratedProductCode(PRODUCT_SKU, PRODUCT_CODE, false);
    Mockito.verify(productOutbound).republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE));
    Mockito.verify(productService).saveMasterProductMigrationHistory(productMigrationArgumentCaptor.getAllValues().get(1));
    Mockito.verify(productMigrationService).save(productMigrationArgumentCaptor.getAllValues().get(2));
    Assertions.assertNotNull(productMigrationArgumentCaptor.getAllValues().get(2).getMigratedProductId());
    Assertions.assertTrue(productMigrationArgumentCaptor.getAllValues().get(2).isL3Updated());
    Assertions.assertTrue(productMigrationArgumentCaptor.getAllValues().get(2).isProductPublished());
    Assertions.assertEquals(1, productMigrationArgumentCaptor.getAllValues().get(2).getRetryCount());
  }

  @Test
  public void retryFailedMigratedProductsPCBUpdateFailedUnSyncTest() throws Exception {
    productMigration.setRetryCount(0);
    productMigration.setSyncStatus(false);
    productMigration.setProductCode(PRODUCT_CODE);
    productMigration.setMerchantType(Constants.CM_MERCHANT);
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    ProductResponse productResponse = new ProductResponse();
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setDescription(new String());
    masterDataProductDTO.setLongDescription(new String());
    productAndItemsResponse.setProduct(productResponse);
    productResponse.setMasterDataProduct(masterDataProductDTO);
    Mockito.when(productMigrationService.findFailedProductMigration(MigrationStatus.FAILED.getMigrationStatus(), 1, 3))
        .thenReturn(new PageImpl<>(Arrays.asList(productMigration)));
    Mockito.when(xProductOutbound.getProductAndItemsWithProductData(true, PRODUCT_SKU, true, false))
        .thenReturn(new GdnRestSingleResponse<>(productAndItemsResponse, REQUEST_ID));
    Mockito.when(productOutbound
        .migrateProduct(Mockito.any(), Mockito.eq(PRODUCT_CODE), Mockito.eq(BUSINESS_PARTNER_ID),
            Mockito.any())).thenReturn(PRODUCT_ID);
    Mockito.doNothing().when(productService)
        .updateProductCollectionForMigratedProduct(productMigrationArgumentCaptor.capture(),
            Mockito.any(ProductAndItemsResponse.class));
    Mockito.doNothing().when(xProductOutbound).updateMigratedProductCode(PRODUCT_SKU, PRODUCT_CODE, false);
    Mockito.when(productOutbound.republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE))).thenReturn(true);
    Mockito.doNothing().when(productService).saveMasterProductMigrationHistory(productMigrationArgumentCaptor.capture());
    Mockito.when(productMigrationService.save(productMigrationArgumentCaptor.capture())).thenReturn(productMigration);
    productMigrationWrapperService.retryFailedMigratedProducts(1, 3);
    Mockito.verify(productMigrationService)
        .findFailedProductMigration(MigrationStatus.FAILED.getMigrationStatus(), 1, 3);
    Mockito.verify(xProductOutbound).getProductAndItemsWithProductData(true, PRODUCT_SKU, true, false);
    Mockito.verify(productOutbound)
        .migrateProduct(Mockito.any(), Mockito.eq(PRODUCT_CODE), Mockito.eq(BUSINESS_PARTNER_ID),
            Mockito.any());
    Mockito.verify(productService)
        .updateProductCollectionForMigratedProduct(Mockito.eq(productMigrationArgumentCaptor.getAllValues().get(0)),
            Mockito.any(ProductAndItemsResponse.class));
    Mockito.verify(xProductOutbound).updateMigratedProductCode(PRODUCT_SKU, PRODUCT_CODE, false);
    Mockito.verify(productOutbound).republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE));
    Mockito.verify(productService).saveMasterProductMigrationHistory(productMigrationArgumentCaptor.getAllValues().get(1));
    Mockito.verify(productMigrationService).save(productMigrationArgumentCaptor.getAllValues().get(2));
    Assertions.assertNotNull(productMigrationArgumentCaptor.getAllValues().get(2).getMigratedProductId());
    Assertions.assertTrue(productMigrationArgumentCaptor.getAllValues().get(2).isL3Updated());
    Assertions.assertTrue(productMigrationArgumentCaptor.getAllValues().get(2).isProductPublished());
    Assertions.assertEquals(1, productMigrationArgumentCaptor.getAllValues().get(2).getRetryCount());
  }

  @Test
  public void retryFailedMigratedProductsL3UpdateFailedTest() throws Exception {
    productMigration.setRetryCount(0);
    productMigration.setSyncStatus(true);
    productMigration.setProductCode(PRODUCT_CODE);
    productMigration.setMigratedProductId(PRODUCT_ID);
    productMigration.setMerchantType(Constants.CM_MERCHANT);
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    ProductResponse productResponse = new ProductResponse();
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setDescription(new String());
    masterDataProductDTO.setLongDescription(new String());
    productAndItemsResponse.setProduct(productResponse);
    productResponse.setMasterDataProduct(masterDataProductDTO);
    Mockito.when(productMigrationService.findFailedProductMigration(MigrationStatus.FAILED.getMigrationStatus(), 1, 3))
        .thenReturn(new PageImpl<>(Arrays.asList(productMigration)));
    Mockito.when(xProductOutbound.getProductAndItemsWithProductData(true, PRODUCT_SKU, true, false))
        .thenReturn(new GdnRestSingleResponse<>(productAndItemsResponse, REQUEST_ID));
    Mockito.when(productOutbound.migrateProduct(PRODUCT_CODE, PRODUCT_CODE, BUSINESS_PARTNER_ID, null))
        .thenReturn(PRODUCT_ID);
    Mockito.doNothing().when(xProductOutbound).updateMigratedProductCode(PRODUCT_SKU, PRODUCT_CODE, false);
    Mockito.when(productOutbound.republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE))).thenReturn(true);
    Mockito.doNothing().when(productService).saveMasterProductMigrationHistory(productMigrationArgumentCaptor.capture());
    Mockito.when(productMigrationService.save(productMigrationArgumentCaptor.capture())).thenReturn(productMigration);
    productMigrationWrapperService.retryFailedMigratedProducts(1, 3);
    Mockito.verify(productMigrationService)
        .findFailedProductMigration(MigrationStatus.FAILED.getMigrationStatus(), 1, 3);
    Mockito.verify(xProductOutbound).getProductAndItemsWithProductData(true, PRODUCT_SKU, true, false);
    Mockito.verify(xProductOutbound).updateMigratedProductCode(PRODUCT_SKU, PRODUCT_CODE, false);
    Mockito.verify(productOutbound).republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE));
    Mockito.verify(productService).saveMasterProductMigrationHistory(productMigrationArgumentCaptor.getAllValues().get(1));
    Mockito.verify(productMigrationService).save(productMigrationArgumentCaptor.getAllValues().get(1));
    Assertions.assertNotNull(productMigrationArgumentCaptor.getAllValues().get(1).getMigratedProductId());
    Assertions.assertTrue(productMigrationArgumentCaptor.getAllValues().get(1).isL3Updated());
    Assertions.assertTrue(productMigrationArgumentCaptor.getAllValues().get(1).isProductPublished());
    Assertions.assertEquals(1, productMigrationArgumentCaptor.getAllValues().get(1).getRetryCount());
  }

  @Test
  public void retryFailedMigratedProductsProductPublishFailedTest() throws Exception {
    productMigration.setRetryCount(0);
    productMigration.setSyncStatus(true);
    productMigration.setProductCode(PRODUCT_CODE);
    productMigration.setMigratedProductId(PRODUCT_ID);
    productMigration.setL3Updated(true);
    productMigration.setMerchantType(Constants.CM_MERCHANT);
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    ProductResponse productResponse = new ProductResponse();
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setDescription(new String());
    masterDataProductDTO.setLongDescription(new String());
    productAndItemsResponse.setProduct(productResponse);
    productResponse.setMasterDataProduct(masterDataProductDTO);
    Mockito.when(xProductOutbound.getProductAndItemsWithProductData(true, PRODUCT_SKU, true, false))
        .thenReturn(new GdnRestSingleResponse<>(productAndItemsResponse, REQUEST_ID));
    Mockito.when(productMigrationService.findFailedProductMigration(MigrationStatus.FAILED.getMigrationStatus(), 1, 3))
        .thenReturn(new PageImpl<>(Arrays.asList(productMigration)));
    Mockito.when(productOutbound.republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE))).thenReturn(true);
    Mockito.doNothing().when(productService).saveMasterProductMigrationHistory(productMigrationArgumentCaptor.capture());
    Mockito.when(productMigrationService.save(productMigrationArgumentCaptor.capture())).thenReturn(productMigration);
    productMigrationWrapperService.retryFailedMigratedProducts(1, 3);
    Mockito.verify(productMigrationService)
        .findFailedProductMigration(MigrationStatus.FAILED.getMigrationStatus(), 1, 3);
    Mockito.verify(productOutbound).republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE));
    Mockito.verify(productService).saveMasterProductMigrationHistory(productMigrationArgumentCaptor.getAllValues().get(1));
    Mockito.verify(xProductOutbound).getProductAndItemsWithProductData(true, PRODUCT_SKU, true, false);
    Mockito.verify(productMigrationService).save(productMigrationArgumentCaptor.getAllValues().get(1));
    Assertions.assertNotNull(productMigrationArgumentCaptor.getAllValues().get(1).getMigratedProductId());
    Assertions.assertTrue(productMigrationArgumentCaptor.getAllValues().get(1).isL3Updated());
    Assertions.assertTrue(productMigrationArgumentCaptor.getAllValues().get(1).isProductPublished());
    Assertions.assertEquals(1, productMigrationArgumentCaptor.getAllValues().get(1).getRetryCount());
  }

  @Test
  public void retryFailedMigratedProductsProductExceptionTest() throws Exception {
    productMigration.setRetryCount(0);
    productMigration.setProductCode(PRODUCT_CODE);
    productMigration.setMigratedProductId(PRODUCT_ID);
    productMigration.setL3Updated(true);
    productMigration.setMerchantType(Constants.CM_MERCHANT);
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    ProductResponse productResponse = new ProductResponse();
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setDescription(new String());
    masterDataProductDTO.setLongDescription(new String());
    productAndItemsResponse.setProduct(productResponse);
    productResponse.setMasterDataProduct(masterDataProductDTO);
    Mockito.when(xProductOutbound.getProductAndItemsWithProductData(true, PRODUCT_SKU, true, false))
        .thenReturn(new GdnRestSingleResponse<>(productAndItemsResponse, REQUEST_ID));
    Mockito.when(productMigrationService.findFailedProductMigration(MigrationStatus.FAILED.getMigrationStatus(), 1, 3))
        .thenReturn(new PageImpl<>(Arrays.asList(productMigration)));
    Mockito.when(productOutbound.republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE)))
        .thenThrow(RuntimeException.class);
    Mockito.doNothing().when(productService).saveMasterProductMigrationHistory(productMigrationArgumentCaptor.capture());
    Mockito.when(productMigrationService.save(productMigrationArgumentCaptor.capture())).thenReturn(productMigration);
    productMigrationWrapperService.retryFailedMigratedProducts(1, 3);
    Mockito.verify(productMigrationService)
        .findFailedProductMigration(MigrationStatus.FAILED.getMigrationStatus(), 1, 3);
    Mockito.verify(productOutbound).republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE));
    Mockito.verify(xProductOutbound).getProductAndItemsWithProductData(true, PRODUCT_SKU, true, false);
    Mockito.verify(productMigrationService).save(productMigrationArgumentCaptor.getAllValues().get(0));
    Assertions.assertFalse(productMigrationArgumentCaptor.getAllValues().get(0).isProductPublished());
    Assertions.assertEquals(1, productMigrationArgumentCaptor.getAllValues().get(0).getRetryCount());
    Assertions.assertEquals(MigrationStatus.FAILED.getMigrationStatus(),
        productMigrationArgumentCaptor.getAllValues().get(0).getMigrationStatus());
  }

  @Test
  public void retryFailedMigratedProductsByProductCodesTest() throws Exception {
    productMigration.setRetryCount(0);
    productMigration.setSyncStatus(true);
    productMigration.setProductCode(PRODUCT_CODE);
    productMigration.setMigratedProductId(PRODUCT_ID);
    productMigration.setL3Updated(true);
    productMigration.setMerchantType(Constants.CM_MERCHANT);
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    ProductResponse productResponse = new ProductResponse();
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setDescription(new String());
    masterDataProductDTO.setLongDescription(new String());
    productAndItemsResponse.setProduct(productResponse);
    productResponse.setMasterDataProduct(masterDataProductDTO);
    Mockito.when(xProductOutbound.getProductAndItemsWithProductData(true, PRODUCT_SKU, true, false))
        .thenReturn(new GdnRestSingleResponse<>(productAndItemsResponse, REQUEST_ID));
    Mockito.when(productMigrationService
        .findByProductCodesWithStatus(Arrays.asList(PRODUCT_CODE), MigrationStatus.FAILED.getMigrationStatus()))
        .thenReturn(Arrays.asList(productMigration));
    Mockito.when(productOutbound.republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE))).thenReturn(true);
    Mockito.doNothing().when(productService).saveMasterProductMigrationHistory(productMigrationArgumentCaptor.capture());
    Mockito.when(productMigrationService.save(productMigrationArgumentCaptor.capture())).thenReturn(productMigration);
    productMigrationWrapperService.retryFailedMigratedProductsByProductCodes(Arrays.asList(PRODUCT_CODE));
    Mockito.verify(productMigrationService)
        .findByProductCodesWithStatus(Arrays.asList(PRODUCT_CODE), MigrationStatus.FAILED.getMigrationStatus());
    Mockito.verify(productOutbound).republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE));
    Mockito.verify(productService).saveMasterProductMigrationHistory(productMigrationArgumentCaptor.getAllValues().get(1));
    Mockito.verify(xProductOutbound).getProductAndItemsWithProductData(true, PRODUCT_SKU, true, false);
    Mockito.verify(productMigrationService).save(productMigrationArgumentCaptor.getAllValues().get(1));
    Assertions.assertNotNull(productMigrationArgumentCaptor.getAllValues().get(1).getMigratedProductId());
    Assertions.assertTrue(productMigrationArgumentCaptor.getAllValues().get(1).isL3Updated());
    Assertions.assertTrue(productMigrationArgumentCaptor.getAllValues().get(1).isProductPublished());
    Assertions.assertEquals(1, productMigrationArgumentCaptor.getAllValues().get(1).getRetryCount());
  }

  @Test
  public void migrateProductByProductCodeEmptyBusinessPartnerResponseTest() throws Exception {
    Mockito.when(productMigrationService.findByProductCodes(Arrays.asList(PRODUCT_CODE)))
        .thenReturn(Arrays.asList(productMigration));
    Mockito.when(xProductOutbound.getBusinessPartnerDetails(Mockito.anyList()))
        .thenReturn(Arrays.asList(new BusinessPartnerResponse()));
    Mockito.when(productMigrationService.save(productMigration)).thenReturn(productMigration);
    Mockito.doNothing().when(productMigrationService)
        .updateProductMigrationStatusByProductCodes(Arrays.asList(PRODUCT_CODE),
            MigrationStatus.IN_PROGRESS.getMigrationStatus());
    productMigrationWrapperService.migrateProductByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productMigrationService).findByProductCodes(Arrays.asList(PRODUCT_CODE));
    Mockito.verify(xProductOutbound).getBusinessPartnerDetails(Mockito.anyList());
    Mockito.verify(productMigrationService).save(productMigration);
    Mockito.verify(productMigrationService).updateProductMigrationStatusByProductCodes(Arrays.asList(PRODUCT_CODE),
        MigrationStatus.IN_PROGRESS.getMigrationStatus());
  }

  @Test
  public void migrateProductByProductCodeEmptyNonCMBusinessPartnerTest() throws Exception {
    Mockito.when(productMigrationService
        .findByProductCodesWithStatus(Arrays.asList(PRODUCT_CODE), MigrationStatus.PENDING.getMigrationStatus()))
        .thenReturn(Arrays.asList(productMigration));
    Mockito.when(xProductOutbound.getBusinessPartnerDetails(Mockito.anyList()))
        .thenReturn(Arrays.asList(businessPartnerResponse));
    Mockito.when(productMigrationService.save(productMigration)).thenReturn(productMigration);
    Mockito.doNothing().when(productMigrationService)
        .updateProductMigrationStatusByProductCodes(Arrays.asList(PRODUCT_CODE),
            MigrationStatus.IN_PROGRESS.getMigrationStatus());
    productMigrationWrapperService.migrateProductByProductCode(PRODUCT_CODE, true);
    Mockito.verify(productMigrationService)
        .findByProductCodesWithStatus(Arrays.asList(PRODUCT_CODE), MigrationStatus.PENDING.getMigrationStatus());
    Mockito.verify(xProductOutbound).getBusinessPartnerDetails(Mockito.anyList());
    Mockito.verify(productMigrationService).save(productMigration);
    Mockito.verify(productMigrationService).updateProductMigrationStatusByProductCodes(Arrays.asList(PRODUCT_CODE),
        MigrationStatus.IN_PROGRESS.getMigrationStatus());
  }

  @Test
  public void migrateProductByProductCodeEmptyCMResignedBusinessPartnerTest() throws Exception {
    businessPartnerResponse.setMerchantType(Constants.CM_MERCHANT);
    businessPartnerResponse.setMerchantStatus("REJECT");
    Mockito.doNothing().when(productMigrationService)
        .updateProductMigrationStatusByProductCodes(Arrays.asList(PRODUCT_CODE),
            MigrationStatus.IN_PROGRESS.getMigrationStatus());
    Mockito.when(productMigrationService.findByProductCodes(Arrays.asList(PRODUCT_CODE)))
        .thenReturn(Arrays.asList(productMigration));
    Mockito.when(xProductOutbound.getBusinessPartnerDetails(Mockito.anyList()))
        .thenReturn(Arrays.asList(businessPartnerResponse));
    Mockito.when(productMigrationService.save(productMigration)).thenReturn(productMigration);
    productMigrationWrapperService.migrateProductByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productMigrationService).findByProductCodes(Arrays.asList(PRODUCT_CODE));
    Mockito.verify(xProductOutbound).getBusinessPartnerDetails(Mockito.anyList());
    Mockito.verify(productMigrationService).save(productMigrationArgumentCaptor.capture());
    Assertions.assertEquals("REJECT", productMigrationArgumentCaptor.getValue().getMerchantStatus());
    Assertions.assertEquals(MigrationStatus.NO_MIGRATION.name(), productMigrationArgumentCaptor.getValue().getMigrationStatus());
    Mockito.verify(productMigrationService).updateProductMigrationStatusByProductCodes(Arrays.asList(PRODUCT_CODE),
        MigrationStatus.IN_PROGRESS.getMigrationStatus());
  }

  @Test
  public void migrateProductByProductCodeEmptyProductSkuTest() throws Exception {
    businessPartnerResponse.setMerchantType(Constants.CM_MERCHANT);
    productMigration.setGdnProductSku(null);
    Mockito.when(productMigrationService.findByProductCodes(Arrays.asList(PRODUCT_CODE)))
        .thenReturn(Arrays.asList(productMigration));
    Mockito.when(xProductOutbound.getBusinessPartnerDetails(Mockito.anyList()))
        .thenReturn(Arrays.asList(businessPartnerResponse));
    Mockito.when(productMigrationService.save(productMigration)).thenReturn(productMigration);
    Mockito.doNothing().when(productMigrationService)
        .updateProductMigrationStatusByProductCodes(Arrays.asList(PRODUCT_CODE),
            MigrationStatus.IN_PROGRESS.getMigrationStatus());
    productMigrationWrapperService.migrateProductByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productMigrationService).findByProductCodes(Arrays.asList(PRODUCT_CODE));
    Mockito.verify(xProductOutbound).getBusinessPartnerDetails(Mockito.anyList());
    Mockito.verify(productMigrationService).save(productMigrationArgumentCaptor.capture());
    Assertions.assertEquals("ACTIVE", productMigrationArgumentCaptor.getValue().getMerchantStatus());
    Assertions.assertEquals(MigrationStatus.NO_MIGRATION.name(), productMigrationArgumentCaptor.getValue().getMigrationStatus());
    Mockito.verify(productMigrationService).updateProductMigrationStatusByProductCodes(Arrays.asList(PRODUCT_CODE),
        MigrationStatus.IN_PROGRESS.getMigrationStatus());
  }

  @Test
  public void migrateProductByProductCodeXproductExceptionTest() throws Exception {
    businessPartnerResponse.setMerchantType(Constants.CM_MERCHANT);
    Mockito.when(productMigrationService.findByProductCodes(Arrays.asList(PRODUCT_CODE)))
        .thenReturn(Arrays.asList(productMigration));
    Mockito.when(xProductOutbound.getBusinessPartnerDetails(Mockito.anyList()))
        .thenReturn(Arrays.asList(businessPartnerResponse));
    Mockito.when(productMigrationService.save(productMigration)).thenReturn(productMigration);
    Mockito.doThrow(RuntimeException.class).when(xProductOutbound)
        .getProductAndItemsWithProductData(true, productMigration.getGdnProductSku(), true, false);
    Mockito.doNothing().when(productMigrationService)
        .updateProductMigrationStatusByProductCodes(Arrays.asList(PRODUCT_CODE),
            MigrationStatus.IN_PROGRESS.getMigrationStatus());
    productMigrationWrapperService.migrateProductByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productMigrationService).findByProductCodes(Arrays.asList(PRODUCT_CODE));
    Mockito.verify(xProductOutbound).getBusinessPartnerDetails(Mockito.anyList());
    Mockito.verify(productMigrationService).save(productMigrationArgumentCaptor.capture());
    Mockito.verify(xProductOutbound).getProductAndItemsWithProductData(true, productMigration.getGdnProductSku(), true,
        false);
    Assertions.assertEquals(MigrationStatus.FAILED.name(),
        productMigrationArgumentCaptor.getValue().getMigrationStatus());
    Mockito.verify(productMigrationService).updateProductMigrationStatusByProductCodes(Arrays.asList(PRODUCT_CODE),
        MigrationStatus.IN_PROGRESS.getMigrationStatus());
  }

  @Test
  public void migrateProductByProductCodeUnsyncProductMFDTrueTest() throws Exception {
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    ProductResponse productResponse = new ProductResponse();
    productResponse.setMarkForDelete(true);
    productAndItemsResponse.setProduct(productResponse);
    businessPartnerResponse.setMerchantType(Constants.CM_MERCHANT);
    Mockito.when(productMigrationService
        .findByProductCodesWithStatus(Arrays.asList(PRODUCT_CODE), MigrationStatus.PENDING.getMigrationStatus()))
        .thenReturn(Arrays.asList(productMigration));
    Mockito.when(xProductOutbound.getBusinessPartnerDetails(Mockito.anyList()))
        .thenReturn(Arrays.asList(businessPartnerResponse));
    Mockito.when(productMigrationService.save(productMigration)).thenReturn(productMigration);
    Mockito.when(xProductOutbound.getProductAndItemsWithProductData(true, productMigration.getGdnProductSku(), true,
            false))
        .thenReturn(new GdnRestSingleResponse<>(productAndItemsResponse, REQUEST_ID));
    Mockito.doNothing().when(productMigrationService)
        .updateProductMigrationStatusByProductCodes(Arrays.asList(PRODUCT_CODE),
            MigrationStatus.IN_PROGRESS.getMigrationStatus());
    productMigrationWrapperService.migrateProductByProductCode(PRODUCT_CODE, true);
    Mockito.verify(productMigrationService)
        .findByProductCodesWithStatus(Arrays.asList(PRODUCT_CODE), MigrationStatus.PENDING.getMigrationStatus());
    Mockito.verify(xProductOutbound).getBusinessPartnerDetails(Mockito.anyList());
    Mockito.verify(productMigrationService).save(productMigrationArgumentCaptor.capture());
    Mockito.verify(xProductOutbound).getProductAndItemsWithProductData(true, productMigration.getGdnProductSku(), true,
        false);
    Assertions.assertEquals(MigrationStatus.NO_MIGRATION.name(),
        productMigrationArgumentCaptor.getValue().getMigrationStatus());
    Mockito.verify(productMigrationService).updateProductMigrationStatusByProductCodes(Arrays.asList(PRODUCT_CODE),
        MigrationStatus.IN_PROGRESS.getMigrationStatus());
  }

  @Test
  public void migrateProductByProductCodeInvalidProductCodeTest() throws Exception {
    Mockito.when(productMigrationService.findByProductCodes(Arrays.asList(PRODUCT_CODE)))
        .thenReturn(new ArrayList<>());
    productMigrationWrapperService.migrateProductByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productMigrationService).findByProductCodes(Arrays.asList(PRODUCT_CODE));
  }

  @Test
  public void migrateProductByProductCodeUnsyncProductTest() throws Exception {
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    ProductResponse productResponse = new ProductResponse();
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setDescription(new String());
    masterDataProductDTO.setLongDescription(new String());
    productAndItemsResponse.setProduct(productResponse);
    productResponse.setMasterDataProduct(masterDataProductDTO);
    businessPartnerResponse.setMerchantType(Constants.CM_MERCHANT);
    Mockito.when(productMigrationService.findByProductCodes(Arrays.asList(PRODUCT_CODE)))
        .thenReturn(Arrays.asList(productMigration, productMigration1));
    Mockito.when(xProductOutbound.getBusinessPartnerDetails(Mockito.anyList()))
        .thenReturn(Arrays.asList(businessPartnerResponse));
    Mockito.when(productMigrationService.save(productMigration)).thenReturn(productMigration);
    Mockito.when(productMigrationService.save(productMigration1)).thenReturn(productMigration1);
    Mockito.when(xProductOutbound.getProductAndItemsWithProductData(true, productMigration.getGdnProductSku(), true,
            false))
        .thenReturn(new GdnRestSingleResponse<>(productAndItemsResponse, REQUEST_ID));
    Mockito.when(xProductOutbound.getProductAndItemsWithProductData(true, productMigration1.getGdnProductSku(), true,
            false))
        .thenReturn(new GdnRestSingleResponse<>(productAndItemsResponse, REQUEST_ID));
    Mockito.doNothing().when(productOutbound).updateProductContent(Mockito.any(ProductRequest.class), Mockito.anyBoolean());
    Mockito.when(productOutbound.migrateProduct(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.any(ProductRequest.class))).thenReturn(PRODUCT_ID);
    Mockito.when(productLevel3Repository.synchronizeProduct(productMigration.getGdnProductSku())).thenReturn(productAndItemsResponse);
    Mockito.doNothing().when(productService).saveSyncHistoryForItems(productMigration);
    Mockito.doNothing().when(productService)
        .updateProductCollectionForMigratedProduct(Mockito.any(ProductMigration.class),
            Mockito.any(ProductAndItemsResponse.class));
    Mockito.doNothing().when(xProductOutbound).updateMigratedProductCode(PRODUCT_SKU, PRODUCT_CODE, false);
    Mockito.when(productOutbound.republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE))).thenReturn(true);
    Mockito.doNothing().when(productService)
        .saveMasterProductMigrationHistory(productMigrationArgumentCaptor.capture());
    Mockito.doNothing().when(productMigrationService)
        .updateProductMigrationStatusByProductCodes(Arrays.asList(PRODUCT_CODE),
            MigrationStatus.IN_PROGRESS.getMigrationStatus());
    Mockito.when(productService.generateProductCode()).thenReturn(PRODUCT_CODE);
    productMigrationWrapperService.migrateProductByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productMigrationService).findByProductCodes(Arrays.asList(PRODUCT_CODE));
    Mockito.verify(xProductOutbound).getBusinessPartnerDetails(Mockito.anyList());
    Mockito.verify(productMigrationService, Mockito.times(4)).save(Mockito.any(ProductMigration.class));
    Mockito.verify(xProductOutbound).getProductAndItemsWithProductData(true, productMigration.getGdnProductSku(), true,
        false);
    Mockito.verify(xProductOutbound).getProductAndItemsWithProductData(true, productMigration1.getGdnProductSku(), true,
        false);
    Mockito.verify(productOutbound, Mockito.times(2)).migrateProduct(Mockito.any(), Mockito.eq(PRODUCT_CODE), Mockito.eq(BUSINESS_PARTNER_ID),
        Mockito.any(ProductRequest.class));
    Mockito.verify(productService).updateProductCollectionForMigratedProduct(Mockito.any(ProductMigration.class),
        Mockito.any(ProductAndItemsResponse.class));
    Mockito.verify(xProductOutbound).updateMigratedProductCode(PRODUCT_SKU, PRODUCT_CODE, false);
    Mockito.verify(productOutbound).republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE));
    Mockito.verify(productService).saveMasterProductMigrationHistory(Mockito.any(ProductMigration.class));
    Mockito.verify(productService).generateProductCode();
    Mockito.verify(productService).saveSyncHistoryForItems(productMigration1);
    Mockito.verify(productMigrationService).updateProductMigrationStatusByProductCodes(Arrays.asList(PRODUCT_CODE),
        MigrationStatus.IN_PROGRESS.getMigrationStatus());
  }

  @Test
  public void migrateProductByProductCodeUnsyncMFDTrueSuspendedTrueProductTest() throws Exception {
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    ProductResponse productResponse = new ProductResponse();
    productResponse.setSuspended(true);
    productResponse.setMarkForDelete(true);
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setDescription(new String());
    masterDataProductDTO.setLongDescription(new String());
    productAndItemsResponse.setProduct(productResponse);
    productResponse.setMasterDataProduct(masterDataProductDTO);
    businessPartnerResponse.setMerchantType(Constants.CM_MERCHANT);
    Mockito.when(productMigrationService.findByProductCodes(Arrays.asList(PRODUCT_CODE)))
        .thenReturn(Arrays.asList(productMigration, productMigration1));
    Mockito.when(xProductOutbound.getBusinessPartnerDetails(Mockito.anyList()))
        .thenReturn(Arrays.asList(businessPartnerResponse));
    Mockito.when(productMigrationService.save(productMigration)).thenReturn(productMigration);
    Mockito.when(productMigrationService.save(productMigration1)).thenReturn(productMigration1);
    Mockito.when(xProductOutbound.getProductAndItemsWithProductData(true, productMigration.getGdnProductSku(), true,
            false))
        .thenReturn(new GdnRestSingleResponse<>(productAndItemsResponse, REQUEST_ID));
    Mockito.when(xProductOutbound.getProductAndItemsWithProductData(true, productMigration1.getGdnProductSku(), true,
            false))
        .thenReturn(new GdnRestSingleResponse<>(productAndItemsResponse, REQUEST_ID));
    Mockito.doNothing().when(productOutbound).updateProductContent(Mockito.any(ProductRequest.class), Mockito.anyBoolean());
    Mockito.doNothing().when(productMigrationService)
        .updateProductMigrationStatusByProductCodes(Arrays.asList(PRODUCT_CODE),
            MigrationStatus.IN_PROGRESS.getMigrationStatus());
    Mockito.when(productOutbound.migrateProduct(Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.any())).thenReturn(PRODUCT_ID);
    Mockito.when(productLevel3Repository.synchronizeProduct(productMigration.getGdnProductSku())).thenReturn(productAndItemsResponse);
    Mockito.doNothing().when(productService).saveSyncHistoryForItems(productMigration);
    Mockito.doNothing().when(productService)
        .updateProductCollectionForMigratedProduct(Mockito.any(), Mockito.any());
    Mockito.doNothing().when(xProductOutbound).updateMigratedProductCode(PRODUCT_SKU, PRODUCT_CODE, false);
    Mockito.when(productOutbound.republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE))).thenReturn(true);
    Mockito.doNothing().when(productService)
        .saveMasterProductMigrationHistory(productMigrationArgumentCaptor.capture());
    Mockito.when(productService.generateProductCode()).thenReturn(PRODUCT_CODE);
    productMigrationWrapperService.migrateProductByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productMigrationService).findByProductCodes(Arrays.asList(PRODUCT_CODE));
    Mockito.verify(xProductOutbound).getBusinessPartnerDetails(Mockito.anyList());
    Mockito.verify(productMigrationService, Mockito.times(4)).save(Mockito.any(ProductMigration.class));
    Mockito.verify(xProductOutbound).getProductAndItemsWithProductData(true, productMigration.getGdnProductSku(), true,
        false);
    Mockito.verify(xProductOutbound).getProductAndItemsWithProductData(true, productMigration1.getGdnProductSku(), true,
        false);
    Mockito.verify(productOutbound, Mockito.times(2)).migrateProduct(Mockito.any(), Mockito.eq(PRODUCT_CODE), Mockito.eq(BUSINESS_PARTNER_ID),
        Mockito.any());
    Mockito.verify(productService).updateProductCollectionForMigratedProduct(Mockito.any(), Mockito.any());
    Mockito.verify(xProductOutbound).updateMigratedProductCode(PRODUCT_SKU, PRODUCT_CODE, false);
    Mockito.verify(productOutbound).republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE));
    Mockito.verify(productService).saveMasterProductMigrationHistory(Mockito.any());
    Mockito.verify(productService).generateProductCode();
    Mockito.verify(productService).saveSyncHistoryForItems(Mockito.any());
    Mockito.verify(productMigrationService).updateProductMigrationStatusByProductCodes(Arrays.asList(PRODUCT_CODE),
        MigrationStatus.IN_PROGRESS.getMigrationStatus());
  }

  @Test
  public void migrateProductByProductCodeUnsyncProductExceptionTest() throws Exception {
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    ProductResponse productResponse = new ProductResponse();
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setDescription(new String());
    masterDataProductDTO.setLongDescription(new String());
    productAndItemsResponse.setProduct(productResponse);
    productResponse.setMasterDataProduct(masterDataProductDTO);
    businessPartnerResponse.setMerchantType(Constants.CM_MERCHANT);
    Mockito.when(productMigrationService.findByProductCodes(Arrays.asList(PRODUCT_CODE)))
        .thenReturn(Arrays.asList(productMigration));
    Mockito.when(xProductOutbound.getBusinessPartnerDetails(Mockito.anyList()))
        .thenReturn(Arrays.asList(businessPartnerResponse));
    Mockito.when(productMigrationService.save(productMigration)).thenReturn(productMigration);
    Mockito.when(xProductOutbound.getProductAndItemsWithProductData(true, productMigration.getGdnProductSku(), true,
            false))
        .thenReturn(new GdnRestSingleResponse<>(productAndItemsResponse, REQUEST_ID));
    Mockito.doThrow(Exception.class).when(productOutbound)
        .migrateProduct(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.any(ProductRequest.class));
    Mockito.when(productService.generateProductCode()).thenReturn(PRODUCT_CODE);
    Mockito.doNothing().when(productMigrationService)
        .updateProductMigrationStatusByProductCodes(Arrays.asList(PRODUCT_CODE),
            MigrationStatus.IN_PROGRESS.getMigrationStatus());
    productMigrationWrapperService.migrateProductByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productMigrationService).findByProductCodes(Arrays.asList(PRODUCT_CODE));
    Mockito.verify(xProductOutbound).getBusinessPartnerDetails(Mockito.anyList());
    Mockito.verify(productMigrationService, Mockito.times(2)).save(Mockito.any(ProductMigration.class));
    Mockito.verify(xProductOutbound).getProductAndItemsWithProductData(true, productMigration.getGdnProductSku(), true,
        false);
    Mockito.verify(productOutbound).migrateProduct(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.any(ProductRequest.class));
    Mockito.verify(productMigrationService).updateProductMigrationStatusByProductCodes(Arrays.asList(PRODUCT_CODE),
        MigrationStatus.IN_PROGRESS.getMigrationStatus());
  }

  @Test
  public void migrateProductByProductCodeUnsyncMFDTrueSuspendedTrueProductExceptionTest() throws Exception {
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    ProductResponse productResponse = new ProductResponse();
    productResponse.setSuspended(true);
    productResponse.setMarkForDelete(true);
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setDescription(new String());
    masterDataProductDTO.setLongDescription(new String());
    productAndItemsResponse.setProduct(productResponse);
    productResponse.setMasterDataProduct(masterDataProductDTO);
    businessPartnerResponse.setMerchantType(Constants.CM_MERCHANT);
    Mockito.when(productMigrationService.findByProductCodes(Arrays.asList(PRODUCT_CODE)))
        .thenReturn(Arrays.asList(productMigration, productMigration1));
    Mockito.when(xProductOutbound.getBusinessPartnerDetails(Mockito.any()))
        .thenReturn(Arrays.asList(businessPartnerResponse));
    Mockito.when(productMigrationService.save(productMigration)).thenReturn(productMigration);
    Mockito.when(productMigrationService.save(productMigration1)).thenReturn(productMigration1);
    Mockito.when(xProductOutbound.getProductAndItemsWithProductData(true, productMigration.getGdnProductSku(), true,
            false))
        .thenReturn(new GdnRestSingleResponse<>(productAndItemsResponse, REQUEST_ID));
    Mockito.when(xProductOutbound.getProductAndItemsWithProductData(true, productMigration1.getGdnProductSku(), true,
            false))
        .thenReturn(new GdnRestSingleResponse<>(productAndItemsResponse, REQUEST_ID));
    Mockito.doNothing().when(productOutbound).updateProductContent(Mockito.any(), Mockito.anyBoolean());
    Mockito.when(productOutbound.migrateProduct(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any())).thenReturn(PRODUCT_ID);
    Mockito.when(productLevel3Repository.synchronizeProduct(productMigration.getGdnProductSku())).thenReturn(productAndItemsResponse);
    Mockito.doNothing().when(productService).saveSyncHistoryForItems(productMigration);
    Mockito.doNothing().when(productService)
        .updateProductCollectionForMigratedProduct(Mockito.any(), Mockito.any());
    Mockito.doNothing().when(xProductOutbound).updateMigratedProductCode(PRODUCT_SKU, PRODUCT_CODE, false);
    Mockito.when(productOutbound.republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE))).thenReturn(true);
    Mockito.doThrow(RuntimeException.class).when(productService)
        .saveMasterProductMigrationHistory(productMigrationArgumentCaptor.capture());
    Mockito.when(productService.generateProductCode()).thenReturn(PRODUCT_CODE);
    Mockito.doNothing().when(productMigrationService)
        .updateProductMigrationStatusByProductCodes(Arrays.asList(PRODUCT_CODE),
            MigrationStatus.IN_PROGRESS.getMigrationStatus());
    productMigrationWrapperService.migrateProductByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productMigrationService).findByProductCodes(Arrays.asList(PRODUCT_CODE));
    Mockito.verify(xProductOutbound).getBusinessPartnerDetails(Mockito.anyList());
    Mockito.verify(productMigrationService, Mockito.times(4)).save(Mockito.any());
    Mockito.verify(xProductOutbound).getProductAndItemsWithProductData(true, productMigration.getGdnProductSku(), true,
        false);
    Mockito.verify(xProductOutbound).getProductAndItemsWithProductData(true, productMigration1.getGdnProductSku(), true,
        false);
    Mockito.verify(productOutbound, Mockito.times(2)).migrateProduct(Mockito.any(), Mockito.eq(PRODUCT_CODE), Mockito.eq(BUSINESS_PARTNER_ID),
        Mockito.any());
    Mockito.verify(productService).updateProductCollectionForMigratedProduct(Mockito.any(), Mockito.any());
    Mockito.verify(xProductOutbound).updateMigratedProductCode(PRODUCT_SKU, PRODUCT_CODE, false);
    Mockito.verify(productOutbound).republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE));
    Mockito.verify(productService).saveMasterProductMigrationHistory(Mockito.any());
    Mockito.verify(productService).generateProductCode();
    Mockito.verify(productService).saveSyncHistoryForItems(productMigration1);
    Mockito.verify(productMigrationService).updateProductMigrationStatusByProductCodes(Arrays.asList(PRODUCT_CODE),
        MigrationStatus.IN_PROGRESS.getMigrationStatus());
  }

  @Test
  public void migrateProductByProductCodeSyncProduct() throws Exception {
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    ProductResponse productResponse = new ProductResponse();
    productResponse.setSynchronized(true);
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setDescription(new String());
    masterDataProductDTO.setLongDescription(new String());
    productAndItemsResponse.setProduct(productResponse);
    productResponse.setMasterDataProduct(masterDataProductDTO);
    businessPartnerResponse.setMerchantType(Constants.CM_MERCHANT);
    Mockito.when(productMigrationService.findByProductCodes(Arrays.asList(PRODUCT_CODE)))
        .thenReturn(Arrays.asList(productMigration, productMigration1));
    Mockito.when(xProductOutbound.getBusinessPartnerDetails(Mockito.anyList()))
        .thenReturn(Arrays.asList(businessPartnerResponse));
    Mockito.when(productMigrationService.save(productMigration)).thenReturn(productMigration);
    Mockito.when(productMigrationService.save(productMigration1)).thenReturn(productMigration1);
    Mockito.doNothing().when(productMigrationService).updateProductMigrationStatusByProductCodes(Arrays.asList(PRODUCT_CODE),
            MigrationStatus.IN_PROGRESS.getMigrationStatus());
    Mockito.when(xProductOutbound.getProductAndItemsWithProductData(true, productMigration.getGdnProductSku(), true,
            false))
        .thenReturn(new GdnRestSingleResponse<>(productAndItemsResponse, REQUEST_ID));
    Mockito.when(xProductOutbound.getProductAndItemsWithProductData(true, productMigration1.getGdnProductSku(), true,
            false))
        .thenReturn(new GdnRestSingleResponse<>(productAndItemsResponse, REQUEST_ID));
    Mockito.when(productOutbound.migrateProduct(Mockito.any(), Mockito.eq(PRODUCT_CODE), Mockito.eq(BUSINESS_PARTNER_ID),
        Mockito.any())).thenReturn(PRODUCT_ID);
    Mockito.doNothing().when(productService).updateProductCollectionForMigratedProduct(Mockito.any(), Mockito.any());
    Mockito.doNothing().when(xProductOutbound).updateMigratedProductCode(PRODUCT_SKU, PRODUCT_CODE, false);
    Mockito.doNothing().when(xProductOutbound).updateMigratedProductCode(PRODUCT_SKU1, PRODUCT_CODE, false);
    Mockito.when(productOutbound.republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE))).thenReturn(true);
    Mockito.doNothing().when(productService).saveMasterProductMigrationHistory(productMigrationArgumentCaptor.capture());
    Mockito.when(productService.generateProductCode()).thenReturn(PRODUCT_CODE);
    productMigrationWrapperService.migrateProductByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productMigrationService).findByProductCodes(Arrays.asList(PRODUCT_CODE));
    Mockito.verify(xProductOutbound).getBusinessPartnerDetails(Mockito.anyList());
    Mockito.verify(productMigrationService, Mockito.times(3)).save(Mockito.any());
    Mockito.verify(xProductOutbound).getProductAndItemsWithProductData(true, productMigration.getGdnProductSku(), true,
        false);
    Mockito.verify(xProductOutbound).getProductAndItemsWithProductData(true, productMigration1.getGdnProductSku(), true,
        false);
    Mockito.verify(productOutbound).migrateProduct(Mockito.any(), Mockito.eq(PRODUCT_CODE), Mockito.eq(BUSINESS_PARTNER_ID), Mockito.any());
    Mockito.verify(productService).updateProductCollectionForMigratedProduct(Mockito.any(), Mockito.any());
    Mockito.verify(xProductOutbound).updateMigratedProductCode(Mockito.eq(PRODUCT_SKU1), Mockito.eq(PRODUCT_CODE), Mockito.eq(false));
    Mockito.verify(productOutbound).republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE));
    Mockito.verify(productService).saveMasterProductMigrationHistory(Mockito.any());
    Mockito.verify(productService).generateProductCode();
    Mockito.verify(productMigrationService).updateProductMigrationStatusByProductCodes(Arrays.asList(PRODUCT_CODE),
        MigrationStatus.IN_PROGRESS.getMigrationStatus());
  }

  @Test
  public void migrateProductByProductCodeSyncProductExceptionTest() throws Exception {
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    ProductResponse productResponse = new ProductResponse();
    productResponse.setSynchronized(true);
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setDescription(new String());
    masterDataProductDTO.setLongDescription(new String());
    productAndItemsResponse.setProduct(productResponse);
    productResponse.setMasterDataProduct(masterDataProductDTO);
    businessPartnerResponse.setMerchantType(Constants.CM_MERCHANT);
    Mockito.when(productMigrationService.findByProductCodes(Arrays.asList(PRODUCT_CODE)))
        .thenReturn(Arrays.asList(productMigration, productMigration1));
    Mockito.when(xProductOutbound.getBusinessPartnerDetails(Mockito.anyList()))
        .thenReturn(Arrays.asList(businessPartnerResponse));
    Mockito.when(productMigrationService.save(productMigration)).thenReturn(productMigration);
    Mockito.when(productMigrationService.save(productMigration1)).thenReturn(productMigration1);
    Mockito.when(xProductOutbound.getProductAndItemsWithProductData(true, productMigration.getGdnProductSku(), true,
            false))
        .thenReturn(new GdnRestSingleResponse<>(productAndItemsResponse, REQUEST_ID));
    Mockito.when(xProductOutbound.getProductAndItemsWithProductData(true, productMigration1.getGdnProductSku(), true,
            false))
        .thenReturn(new GdnRestSingleResponse<>(productAndItemsResponse, REQUEST_ID));
    Mockito.when(productOutbound.migrateProduct(Mockito.anyString(), Mockito.eq(PRODUCT_CODE), Mockito.eq(BUSINESS_PARTNER_ID),
        Mockito.any(ProductRequest.class))).thenReturn(PRODUCT_ID);
    Mockito.doNothing().when(productService)
        .updateProductCollectionForMigratedProduct(Mockito.any(ProductMigration.class),
            Mockito.any(ProductAndItemsResponse.class));
    Mockito.doNothing().when(xProductOutbound).updateMigratedProductCode(PRODUCT_SKU, PRODUCT_CODE, false);
    Mockito.doNothing().when(xProductOutbound).updateMigratedProductCode(PRODUCT_SKU1, PRODUCT_CODE, false);
    Mockito.when(productOutbound.republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE))).thenReturn(true);
    Mockito.doThrow(RuntimeException.class).when(productService).saveMasterProductMigrationHistory(productMigrationArgumentCaptor.capture());
    Mockito.when(productService.generateProductCode()).thenReturn(PRODUCT_CODE);
    Mockito.doNothing().when(productMigrationService)
        .updateProductMigrationStatusByProductCodes(Arrays.asList(PRODUCT_CODE),
            MigrationStatus.IN_PROGRESS.getMigrationStatus());
    productMigrationWrapperService.migrateProductByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productMigrationService).findByProductCodes(Arrays.asList(PRODUCT_CODE));
    Mockito.verify(xProductOutbound).getBusinessPartnerDetails(Mockito.anyList());
    Mockito.verify(productMigrationService, Mockito.times(3)).save(Mockito.any(ProductMigration.class));
    Mockito.verify(xProductOutbound).getProductAndItemsWithProductData(true, productMigration.getGdnProductSku(), true,
        false);
    Mockito.verify(xProductOutbound).getProductAndItemsWithProductData(true, productMigration1.getGdnProductSku(), true,
        false);
    Mockito.verify(productOutbound).migrateProduct(Mockito.anyString(), Mockito.eq(PRODUCT_CODE), Mockito.eq(BUSINESS_PARTNER_ID),
        Mockito.any());
    Mockito.verify(productService).updateProductCollectionForMigratedProduct(Mockito.any(ProductMigration.class),
        Mockito.any());
    Mockito.verify(xProductOutbound).updateMigratedProductCode(Mockito.eq(PRODUCT_SKU1), Mockito.eq(PRODUCT_CODE), Mockito.eq(false));
    Mockito.verify(productOutbound).republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE));
    Mockito.verify(productService).saveMasterProductMigrationHistory(Mockito.any(ProductMigration.class));
    Mockito.verify(productService).generateProductCode();
    Mockito.verify(productMigrationService).updateProductMigrationStatusByProductCodes(Arrays.asList(PRODUCT_CODE),
        MigrationStatus.IN_PROGRESS.getMigrationStatus());
  }

  @Test
  public void migrateProductByProductSkusTest() throws Exception {
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    ProductResponse productResponse = new ProductResponse();
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setDescription(new String());
    masterDataProductDTO.setLongDescription(new String());
    productAndItemsResponse.setProduct(productResponse);
    productResponse.setMasterDataProduct(masterDataProductDTO);
    businessPartnerResponse.setMerchantType(Constants.CM_MERCHANT);
    Mockito.when(xProductOutbound.getBusinessPartnerDetails(Mockito.anyList()))
        .thenReturn(Arrays.asList(businessPartnerResponse));
    Mockito.when(productMigrationService.save(productMigration)).thenReturn(productMigration);
    Mockito.when(xProductOutbound.getProductAndItemsWithProductData(true, productMigration.getGdnProductSku(), true,
            false))
        .thenReturn(new GdnRestSingleResponse<>(productAndItemsResponse, REQUEST_ID));
    Mockito.when(productOutbound.migrateProduct(Mockito.anyString(), Mockito.eq(PRODUCT_CODE), Mockito.eq(BUSINESS_PARTNER_ID),
        Mockito.any(ProductRequest.class))).thenReturn(PRODUCT_ID);
    Mockito.doNothing().when(productService)
        .updateProductCollectionForMigratedProduct(Mockito.any(ProductMigration.class),
            Mockito.any(ProductAndItemsResponse.class));
    Mockito.doNothing().when(xProductOutbound).updateMigratedProductCode(PRODUCT_SKU, PRODUCT_CODE, false);
    Mockito.when(productOutbound.republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE))).thenReturn(true);
    Mockito.doNothing().when(productService).saveMasterProductMigrationHistory(productMigrationArgumentCaptor.capture());
    Mockito.when(productService.generateProductCode()).thenReturn(PRODUCT_CODE);
    Mockito.when(productMigrationService
        .getProductMigrationByProductSkusWithStatus(Arrays.asList(productMigration.getGdnProductSku()),
            MigrationStatus.PENDING.getMigrationStatus())).thenReturn(Arrays.asList(productMigration));
    productMigrationWrapperService.migrateProductByProductSkus(Arrays.asList(productMigration.getGdnProductSku()));
    Mockito.verify(productMigrationService).getProductMigrationByProductSkusWithStatus(
        Arrays.asList(productMigration.getGdnProductSku()), MigrationStatus.PENDING.getMigrationStatus());
    Mockito.verify(xProductOutbound).getBusinessPartnerDetails(Mockito.anyList());
    Mockito.verify(productMigrationService, Mockito.times(2)).save(Mockito.any(ProductMigration.class));
    Mockito.verify(xProductOutbound).getProductAndItemsWithProductData(true, productMigration.getGdnProductSku(), true,
        false);
    Mockito.verify(productOutbound).migrateProduct(Mockito.any(), Mockito.eq(PRODUCT_CODE), Mockito.eq(BUSINESS_PARTNER_ID),
        Mockito.any(ProductRequest.class));
    Mockito.verify(productService).updateProductCollectionForMigratedProduct(Mockito.any(ProductMigration.class),
        Mockito.any(ProductAndItemsResponse.class));
    Mockito.verify(xProductOutbound).updateMigratedProductCode(PRODUCT_SKU, PRODUCT_CODE, false);
    Mockito.verify(productOutbound).republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE));
    Mockito.verify(productService).saveMasterProductMigrationHistory(Mockito.any(ProductMigration.class));
    Mockito.verify(productService).generateProductCode();
  }



  @Test
  public void migrateProductByProductSkusEmptyBusinessPartnerResponseTest() throws Exception {
    Mockito.when(xProductOutbound.getBusinessPartnerDetails(Mockito.anyList()))
        .thenReturn(Arrays.asList(new BusinessPartnerResponse()));
    Mockito.when(productMigrationService.save(productMigration)).thenReturn(productMigration);
    Mockito.when(productMigrationService
        .getProductMigrationByProductSkusWithStatus(Arrays.asList(productMigration.getGdnProductSku()),
            MigrationStatus.PENDING.getMigrationStatus())).thenReturn(Arrays.asList(productMigration));
    productMigrationWrapperService.migrateProductByProductSkus(Arrays.asList(productMigration.getGdnProductSku()));
    Mockito.verify(productMigrationService)
        .getProductMigrationByProductSkusWithStatus(Arrays.asList(productMigration.getGdnProductSku()),
            MigrationStatus.PENDING.getMigrationStatus());
    Mockito.verify(xProductOutbound).getBusinessPartnerDetails(Mockito.anyList());
    Mockito.verify(productMigrationService).save(productMigrationArgumentCaptor.capture());
    Assertions.assertEquals(MigrationStatus.NO_MIGRATION.name(),
        productMigrationArgumentCaptor.getValue().getMigrationStatus());
  }

  @Test
  public void migrateProductByProductSkusEmptyResignedMerchant() throws Exception {
    businessPartnerResponse.setMerchantStatus("REJECT");
    Mockito.when(xProductOutbound.getBusinessPartnerDetails(Mockito.anyList()))
        .thenReturn(Arrays.asList(businessPartnerResponse));
    Mockito.when(productMigrationService.save(productMigration)).thenReturn(productMigration);
    Mockito.when(productMigrationService
        .getProductMigrationByProductSkusWithStatus(Arrays.asList(productMigration.getGdnProductSku()),
            MigrationStatus.PENDING.getMigrationStatus())).thenReturn(Arrays.asList(productMigration));
    productMigrationWrapperService.migrateProductByProductSkus(Arrays.asList(productMigration.getGdnProductSku()));
    Mockito.verify(productMigrationService)
        .getProductMigrationByProductSkusWithStatus(Arrays.asList(productMigration.getGdnProductSku()),
            MigrationStatus.PENDING.getMigrationStatus());
    Mockito.verify(xProductOutbound).getBusinessPartnerDetails(Mockito.anyList());
    Mockito.verify(productMigrationService).save(productMigrationArgumentCaptor.capture());
    Assertions.assertEquals(MigrationStatus.NO_MIGRATION.name(),
        productMigrationArgumentCaptor.getValue().getMigrationStatus());
  }

  @Test
  public void migrateProductByProductSkusMFDTrueTest() throws Exception {
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    ProductResponse productResponse = new ProductResponse();
    productResponse.setMarkForDelete(true);
    productAndItemsResponse.setProduct(productResponse);
    businessPartnerResponse.setMerchantType(Constants.CM_MERCHANT);
    Mockito.when(xProductOutbound.getBusinessPartnerDetails(Mockito.anyList()))
        .thenReturn(Arrays.asList(businessPartnerResponse));
    Mockito.when(productMigrationService.save(productMigration)).thenReturn(productMigration);
    Mockito.when(xProductOutbound.getProductAndItemsWithProductData(true, productMigration.getGdnProductSku(), true,
            false))
        .thenReturn(new GdnRestSingleResponse<>(productAndItemsResponse, REQUEST_ID));
    Mockito.when(productMigrationService
        .getProductMigrationByProductSkusWithStatus(Arrays.asList(productMigration.getGdnProductSku()),
            MigrationStatus.PENDING.getMigrationStatus())).thenReturn(Arrays.asList(productMigration));
    productMigrationWrapperService.migrateProductByProductSkus(Arrays.asList(productMigration.getGdnProductSku()));
    Mockito.verify(productMigrationService)
        .getProductMigrationByProductSkusWithStatus(Arrays.asList(productMigration.getGdnProductSku()),
            MigrationStatus.PENDING.getMigrationStatus());
    Mockito.verify(xProductOutbound).getBusinessPartnerDetails(Mockito.anyList());
    Mockito.verify(productMigrationService).save(productMigrationArgumentCaptor.capture());
    Mockito.verify(xProductOutbound).getProductAndItemsWithProductData(true, productMigration.getGdnProductSku(), true,
        false);
    Assertions.assertEquals(MigrationStatus.NO_MIGRATION.name(),
        productMigrationArgumentCaptor.getValue().getMigrationStatus());
  }

  @Test
  public void migrateProductByProductSkusXProductExceptionTest() throws Exception {
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    ProductResponse productResponse = new ProductResponse();
    productAndItemsResponse.setProduct(productResponse);
    businessPartnerResponse.setMerchantType(Constants.CM_MERCHANT);
    Mockito.when(xProductOutbound.getBusinessPartnerDetails(Mockito.anyList()))
        .thenReturn(Arrays.asList(businessPartnerResponse));
    Mockito.when(productMigrationService.save(productMigration)).thenReturn(productMigration);
    Mockito.doThrow(RuntimeException.class).when(xProductOutbound)
        .getProductAndItemsWithProductData(true, productMigration.getGdnProductSku(), true, false);
    Mockito.when(productMigrationService
        .getProductMigrationByProductSkusWithStatus(Arrays.asList(productMigration.getGdnProductSku()),
            MigrationStatus.PENDING.getMigrationStatus())).thenReturn(Arrays.asList(productMigration));
    productMigrationWrapperService.migrateProductByProductSkus(Arrays.asList(productMigration.getGdnProductSku()));
    Mockito.verify(productMigrationService)
        .getProductMigrationByProductSkusWithStatus(Arrays.asList(productMigration.getGdnProductSku()),
            MigrationStatus.PENDING.getMigrationStatus());
    Mockito.verify(xProductOutbound).getBusinessPartnerDetails(Mockito.anyList());
    Mockito.verify(productMigrationService).save(productMigrationArgumentCaptor.capture());
    Mockito.verify(xProductOutbound).getProductAndItemsWithProductData(true, productMigration.getGdnProductSku(), true,
        false);
    Assertions.assertEquals(MigrationStatus.FAILED.name(), productMigrationArgumentCaptor.getValue().getMigrationStatus());
  }

  @Test
  public void migrateProductByProductSkusMFDTrueSuspendedTrueTest() throws Exception {
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    ProductResponse productResponse = new ProductResponse();
    productResponse.setMarkForDelete(true);
    productResponse.setSuspended(true);
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setDescription(new String());
    masterDataProductDTO.setLongDescription(new String());
    productAndItemsResponse.setProduct(productResponse);
    productResponse.setMasterDataProduct(masterDataProductDTO);
    businessPartnerResponse.setMerchantType(Constants.CM_MERCHANT);
    Mockito.when(xProductOutbound.getBusinessPartnerDetails(Mockito.anyList()))
        .thenReturn(Arrays.asList(businessPartnerResponse));
    Mockito.when(productMigrationService.save(productMigration)).thenReturn(productMigration);
    Mockito.when(xProductOutbound.getProductAndItemsWithProductData(true, productMigration.getGdnProductSku(), true,
            false))
        .thenReturn(new GdnRestSingleResponse<>(productAndItemsResponse, REQUEST_ID));
    Mockito.when(productOutbound
        .migrateProduct(Mockito.anyString(), Mockito.eq(PRODUCT_CODE), Mockito.eq(BUSINESS_PARTNER_ID),
            Mockito.any(ProductRequest.class))).thenReturn(PRODUCT_ID);
    Mockito.doNothing().when(productService)
        .updateProductCollectionForMigratedProduct(Mockito.any(ProductMigration.class),
            Mockito.any(ProductAndItemsResponse.class));
    Mockito.doNothing().when(xProductOutbound).updateMigratedProductCode(PRODUCT_SKU, PRODUCT_CODE, false);
    Mockito.when(productOutbound.republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE))).thenReturn(true);
    Mockito.doNothing().when(productService)
        .saveMasterProductMigrationHistory(productMigrationArgumentCaptor.capture());
    Mockito.when(productService.generateProductCode()).thenReturn(PRODUCT_CODE);
    Mockito.when(productMigrationService
        .getProductMigrationByProductSkusWithStatus(Arrays.asList(productMigration.getGdnProductSku()),
            MigrationStatus.PENDING.getMigrationStatus())).thenReturn(Arrays.asList(productMigration));
    productMigrationWrapperService.migrateProductByProductSkus(Arrays.asList(productMigration.getGdnProductSku()));
    Mockito.verify(productMigrationService)
        .getProductMigrationByProductSkusWithStatus(Arrays.asList(productMigration.getGdnProductSku()),
            MigrationStatus.PENDING.getMigrationStatus());
    Mockito.verify(xProductOutbound).getBusinessPartnerDetails(Mockito.anyList());
    Mockito.verify(productMigrationService, Mockito.times(2)).save(Mockito.any(ProductMigration.class));
    Mockito.verify(xProductOutbound).getProductAndItemsWithProductData(true, productMigration.getGdnProductSku(), true,
        false);
    Mockito.verify(productOutbound)
        .migrateProduct(Mockito.any(), Mockito.eq(PRODUCT_CODE), Mockito.eq(BUSINESS_PARTNER_ID),
            Mockito.any(ProductRequest.class));
    Mockito.verify(productService).updateProductCollectionForMigratedProduct(Mockito.any(ProductMigration.class),
        Mockito.any(ProductAndItemsResponse.class));
    Mockito.verify(xProductOutbound).updateMigratedProductCode(PRODUCT_SKU, PRODUCT_CODE, false);
    Mockito.verify(productOutbound).republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE));
    Mockito.verify(productService).saveMasterProductMigrationHistory(Mockito.any(ProductMigration.class));
    Mockito.verify(productService).generateProductCode();
  }

  @Test
  public void migrateProductByProductCodeSyncProductsForDifferentMerchant() throws Exception {
    productMigration1.setBusinessPartnerId(BUSINESS_PARTNER_ID1);
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    ProductResponse productResponse = new ProductResponse();
    productResponse.setSynchronized(true);
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setDescription(new String());
    masterDataProductDTO.setLongDescription(new String());
    productAndItemsResponse.setProduct(productResponse);
    productResponse.setMasterDataProduct(masterDataProductDTO);
    Mockito.doNothing().when(productMigrationService)
        .updateProductMigrationStatusByProductCodes(Arrays.asList(PRODUCT_CODE),
            MigrationStatus.IN_PROGRESS.getMigrationStatus());
    Mockito.when(productMigrationService.findByProductCodes(Arrays.asList(PRODUCT_CODE)))
        .thenReturn(Arrays.asList(productMigration, productMigration1));
    Mockito.when(xProductOutbound.getBusinessPartnerDetails(Mockito.anyList()))
        .thenReturn(Arrays.asList(businessPartnerResponse, businessPartnerResponse1));
    Mockito.when(productMigrationService.save(productMigration)).thenReturn(productMigration);
    Mockito.when(productMigrationService.save(productMigration1)).thenReturn(productMigration1);
    Mockito.when(xProductOutbound.getProductAndItemsWithProductData(true, productMigration.getGdnProductSku(), true,
            false))
        .thenReturn(new GdnRestSingleResponse<>(productAndItemsResponse, REQUEST_ID));
    Mockito.when(xProductOutbound.getProductAndItemsWithProductData(true, productMigration1.getGdnProductSku(), true,
            false))
        .thenReturn(new GdnRestSingleResponse<>(productAndItemsResponse, REQUEST_ID));
    Mockito.when(productOutbound.migrateProduct(Mockito.anyString(), Mockito.eq(PRODUCT_CODE), Mockito.eq(BUSINESS_PARTNER_ID1),
       Mockito.eq(null))).thenReturn(PRODUCT_ID);
    Mockito.doNothing().when(productService)
        .updateProductCollectionForMigratedProduct(Mockito.any(ProductMigration.class),
            Mockito.any(ProductAndItemsResponse.class));
    Mockito.doNothing().when(xProductOutbound).updateMigratedProductCode(PRODUCT_SKU, PRODUCT_CODE, false);
    Mockito.doNothing().when(xProductOutbound).updateMigratedProductCode(PRODUCT_SKU1, PRODUCT_CODE, false);
    Mockito.when(productOutbound.republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE))).thenReturn(true);
    Mockito.doNothing().when(productService).saveMasterProductMigrationHistory(productMigrationArgumentCaptor.capture());
    Mockito.when(productService.generateProductCode()).thenReturn(PRODUCT_CODE);
    productMigrationWrapperService.migrateProductByProductCode(PRODUCT_CODE, false);
    Mockito.verify(productMigrationService).findByProductCodes(Arrays.asList(PRODUCT_CODE));
    Mockito.verify(xProductOutbound).getBusinessPartnerDetails(Mockito.anyList());
    Mockito.verify(productMigrationService, Mockito.times(3)).save(Mockito.any(ProductMigration.class));
    Mockito.verify(xProductOutbound).getProductAndItemsWithProductData(true, productMigration1.getGdnProductSku(), true,
        false);
    Mockito.verify(productOutbound).migrateProduct(Mockito.anyString(), Mockito.eq(PRODUCT_CODE), Mockito.eq(BUSINESS_PARTNER_ID1),
       Mockito.eq(null));
    Mockito.verify(productService).updateProductCollectionForMigratedProduct(Mockito.any(ProductMigration.class), Mockito.any());
    Mockito.verify(xProductOutbound).updateMigratedProductCode(Mockito.eq(PRODUCT_SKU1), Mockito.eq(PRODUCT_CODE), Mockito.eq(false));
    Mockito.verify(productOutbound).republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE));
    Mockito.verify(productService).saveMasterProductMigrationHistory(Mockito.any(ProductMigration.class));
    Mockito.verify(productService).generateProductCode();
    Mockito.verify(productMigrationService).updateProductMigrationStatusByProductCodes(Arrays.asList(PRODUCT_CODE),
        MigrationStatus.IN_PROGRESS.getMigrationStatus());
  }

  @Test
  public void retryFailedMigratedProductsL3UpdateEmptyMigrationStatusTest() throws Exception {
    productMigration.setRetryCount(0);
    productMigration.setSyncStatus(true);
    productMigration.setProductCode(PRODUCT_CODE);
    productMigration.setMigratedProductId(PRODUCT_ID);
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    ProductResponse productResponse = new ProductResponse();
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setDescription(new String());
    masterDataProductDTO.setLongDescription(new String());
    productAndItemsResponse.setProduct(productResponse);
    productResponse.setMasterDataProduct(masterDataProductDTO);
    Mockito.when(productMigrationService.findFailedProductMigration(MigrationStatus.FAILED.getMigrationStatus(), 1, 3))
        .thenReturn(new PageImpl<>(Arrays.asList(productMigration)));
    Mockito.when(xProductOutbound.getProductAndItemsWithProductData(true, PRODUCT_SKU, true, false))
        .thenReturn(new GdnRestSingleResponse<>(productAndItemsResponse, REQUEST_ID));
    Mockito.when(productOutbound.migrateProduct(PRODUCT_CODE, PRODUCT_CODE, BUSINESS_PARTNER_ID, null))
        .thenReturn(PRODUCT_ID);
    Mockito.when(xProductOutbound.getBusinessPartnerDetails(Mockito.anyList()))
        .thenReturn(Arrays.asList(businessPartnerResponse));
    Mockito.doNothing().when(xProductOutbound).updateMigratedProductCode(PRODUCT_SKU, PRODUCT_CODE, false);
    Mockito.when(productOutbound.republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE))).thenReturn(true);
    Mockito.doNothing().when(productService)
        .saveMasterProductMigrationHistory(productMigrationArgumentCaptor.capture());
    Mockito.when(productMigrationService.save(productMigrationArgumentCaptor.capture())).thenReturn(productMigration);
    productMigrationWrapperService.retryFailedMigratedProducts(1, 3);
    Mockito.verify(productMigrationService)
        .findFailedProductMigration(MigrationStatus.FAILED.getMigrationStatus(), 1, 3);
    Mockito.verify(xProductOutbound).getProductAndItemsWithProductData(true, PRODUCT_SKU, true, false);
    Mockito.verify(xProductOutbound).getBusinessPartnerDetails(Mockito.anyList());
    Mockito.verify(productMigrationService).save(Mockito.any(ProductMigration.class));
  }

  @Test
  public void retryFailedMigratedProductsPCBUpdateFailedSyncTest1() throws Exception {
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    ProductResponse productResponse = new ProductResponse();
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setDescription(new String());
    masterDataProductDTO.setLongDescription(new String());
    productAndItemsResponse.setProduct(productResponse);
    productResponse.setMasterDataProduct(masterDataProductDTO);
    productMigration.setRetryCount(0);
    productMigration.setSyncStatus(true);
    productMigration.setProductCode(PRODUCT_CODE);
    Mockito.when(xProductOutbound.getBusinessPartnerDetails(Mockito.anyList()))
        .thenReturn(Arrays.asList(businessPartnerResponse1));
    Mockito.when(productMigrationService.findFailedProductMigration(MigrationStatus.FAILED.getMigrationStatus(), 1, 3))
        .thenReturn(new PageImpl<>(Arrays.asList(productMigration)));
    Mockito.when(productOutbound.migrateProduct(PRODUCT_CODE, PRODUCT_CODE, BUSINESS_PARTNER_ID, null))
        .thenReturn(PRODUCT_ID);
    Mockito.when(xProductOutbound.getProductAndItemsWithProductData(true, PRODUCT_SKU, true, false))
        .thenReturn(new GdnRestSingleResponse<>(null, REQUEST_ID));
    Mockito.doNothing().when(productService)
        .updateProductCollectionForMigratedProduct(productMigrationArgumentCaptor.capture(), Mockito.any());
    Mockito.doNothing().when(xProductOutbound).updateMigratedProductCode(PRODUCT_SKU, PRODUCT_CODE, false);
    Mockito.when(productOutbound.republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE))).thenReturn(true);
    Mockito.doNothing().when(productService).saveMasterProductMigrationHistory(productMigrationArgumentCaptor.capture());
    Mockito.when(productMigrationService.save(productMigrationArgumentCaptor.capture())).thenReturn(productMigration);
    productMigrationWrapperService.retryFailedMigratedProducts(1, 3);
    Mockito.verify(productMigrationService)
        .findFailedProductMigration(MigrationStatus.FAILED.getMigrationStatus(), 1, 3);
    Mockito.verify(xProductOutbound).getProductAndItemsWithProductData(true, PRODUCT_SKU, true, false);
    Mockito.verify(productOutbound).migrateProduct(PRODUCT_CODE, PRODUCT_CODE, BUSINESS_PARTNER_ID, null);
    Mockito.verify(productService)
        .updateProductCollectionForMigratedProduct(productMigrationArgumentCaptor.getAllValues().get(0), null);
    Mockito.verify(xProductOutbound).updateMigratedProductCode(PRODUCT_SKU, PRODUCT_CODE, false);
    Mockito.verify(productOutbound).republishProduct(Constants.MIGRATION, Arrays.asList(PRODUCT_CODE));
    Mockito.verify(productService).saveMasterProductMigrationHistory(productMigrationArgumentCaptor.getAllValues().get(1));
    Mockito.verify(productMigrationService).save(productMigrationArgumentCaptor.getAllValues().get(2));
    Mockito.verify(xProductOutbound).getBusinessPartnerDetails(Mockito.anyList());
    Assertions.assertNotNull(productMigrationArgumentCaptor.getAllValues().get(2).getMigratedProductId());
    Assertions.assertTrue(productMigrationArgumentCaptor.getAllValues().get(2).isL3Updated());
    Assertions.assertTrue(productMigrationArgumentCaptor.getAllValues().get(2).isProductPublished());
    Assertions.assertEquals(1, productMigrationArgumentCaptor.getAllValues().get(2).getRetryCount());
  }

  @Test
  public void retryFailedMigratedProductsL3UpdateEmptyMigrationStatusTest1() throws Exception {
    productMigration.setRetryCount(0);
    productMigration.setSyncStatus(true);
    productMigration.setProductCode(PRODUCT_CODE);
    productMigration.setMigratedProductId(PRODUCT_ID);
    businessPartnerResponse.setMerchantStatus("REJECT");
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    ProductResponse productResponse = new ProductResponse();
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setDescription(new String());
    masterDataProductDTO.setLongDescription(new String());
    productAndItemsResponse.setProduct(productResponse);
    productResponse.setMasterDataProduct(masterDataProductDTO);
    Mockito.when(productMigrationService.findFailedProductMigration(MigrationStatus.FAILED.getMigrationStatus(), 1, 3))
        .thenReturn(new PageImpl<>(Arrays.asList(productMigration)));
    Mockito.when(xProductOutbound.getProductAndItemsWithProductData(true, PRODUCT_SKU, true, false))
        .thenReturn(new GdnRestSingleResponse<>(productAndItemsResponse, REQUEST_ID));
    Mockito.when(productOutbound.migrateProduct(PRODUCT_CODE, PRODUCT_CODE, BUSINESS_PARTNER_ID, null))
        .thenReturn(PRODUCT_ID);
    Mockito.when(xProductOutbound.getBusinessPartnerDetails(Mockito.anyList()))
        .thenReturn(Arrays.asList(businessPartnerResponse));
    Mockito.when(productMigrationService.save(productMigrationArgumentCaptor.capture())).thenReturn(productMigration);
    productMigrationWrapperService.retryFailedMigratedProducts(1, 3);
    Mockito.verify(productMigrationService)
        .findFailedProductMigration(MigrationStatus.FAILED.getMigrationStatus(), 1, 3);
    Mockito.verify(xProductOutbound).getProductAndItemsWithProductData(true, PRODUCT_SKU, true, false);
    Mockito.verify(xProductOutbound).getBusinessPartnerDetails(Mockito.anyList());
    Mockito.verify(productMigrationService).save(Mockito.any(ProductMigration.class));
  }

  @Test
  public void migrateProductAndL5DetailsByProductSkuForActiveProductTest() throws Exception {
    ProductAndL5MigrationRequest productAndL5MigrationRequest =
      ProductAndL5MigrationRequest.builder().productSku(PRODUCT_SKU)
        .productType(ProductType.BIG_PRODUCT).build();
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setState(ProductLevel1State.ACTIVE);
    productBusinessPartner.setGdnProductSku(PRODUCT_SKU);
    productBusinessPartner.setProductId(PRODUCT_ID);
    Mockito.when(productBusinessPartnerService.findFirstByProductSku(PRODUCT_SKU)).thenReturn(productBusinessPartner);
    com.gdn.x.product.rest.web.model.request.ProductAndL5MigrationRequest request =
      new com.gdn.x.product.rest.web.model.request.ProductAndL5MigrationRequest();
    BeanUtils.copyProperties(productAndL5MigrationRequest, request);
    request.setProductType(com.gdn.x.product.enums.ProductType.BIG_PRODUCT);
    Mockito.doNothing().when(xProductOutbound).migrateProductAndL5DetailByProductSku(request,
      Constants.DEFAULT_STORE_ID, Constants.DEFAULT_USERNAME);
    productMigrationWrapperService.migrateProductAndL5DetailsByProductSku(Constants.DEFAULT_STORE_ID,
      productAndL5MigrationRequest, Constants.DEFAULT_USERNAME);
    Mockito.verify(productService).migrateProductAndL5Details(Constants.DEFAULT_STORE_ID,
      productAndL5MigrationRequest, Constants.DEFAULT_USERNAME);
  }

  @Test
  public void migrateProductAndL5DetailsByProductSkuForInProcessProductTest() throws Exception {
    ProductAndL5MigrationRequest productAndL5MigrationRequest =
      ProductAndL5MigrationRequest.builder().productSku(PRODUCT_SKU)
        .productType(ProductType.BIG_PRODUCT).build();
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setState(Constants.IN_PROGRESS_STATE);
    productBusinessPartner.setGdnProductSku(PRODUCT_SKU);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productBusinessPartner.setId(PRODUCT_ID);
    Mockito.when(productBusinessPartnerService.findFirstByProductSku(PRODUCT_SKU)).thenReturn(productBusinessPartner);
    com.gdn.x.product.rest.web.model.request.ProductAndL5MigrationRequest request =
      new com.gdn.x.product.rest.web.model.request.ProductAndL5MigrationRequest();
    BeanUtils.copyProperties(productAndL5MigrationRequest, request);
    Mockito.doNothing().when(productService)
      .migrateProductAndL5DetailsByProductSku(Constants.DEFAULT_STORE_ID, PRODUCT_ID,
        productAndL5MigrationRequest, Constants.DEFAULT_USERNAME, false, false);
    productMigrationWrapperService.migrateProductAndL5DetailsByProductSku(Constants.STORE_ID,
      productAndL5MigrationRequest, Constants.DEFAULT_USERNAME);
    Mockito.verify(productService).migrateProductAndL5Details(Constants.STORE_ID,
      productAndL5MigrationRequest, Constants.DEFAULT_USERNAME);
  }

  @Test
  public void migrateProductAndL5DetailsByProductSkuForNeedRevisionProductTest() throws Exception {
    ProductAndL5MigrationRequest productAndL5MigrationRequest =
      ProductAndL5MigrationRequest.builder().productSku(PRODUCT_SKU)
        .productType(ProductType.BIG_PRODUCT).build();
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setState(ProductLevel1State.NEED_CORRECTION);
    productBusinessPartner.setGdnProductSku(PRODUCT_SKU);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productBusinessPartner.setId(PRODUCT_ID);
    Mockito.when(productBusinessPartnerService.findFirstByProductSku(PRODUCT_SKU)).thenReturn(productBusinessPartner);
    com.gdn.x.product.rest.web.model.request.ProductAndL5MigrationRequest request =
      new com.gdn.x.product.rest.web.model.request.ProductAndL5MigrationRequest();
    BeanUtils.copyProperties(productAndL5MigrationRequest, request);
    Mockito.doNothing().when(productService)
      .migrateProductAndL5DetailsByProductSku(Constants.DEFAULT_STORE_ID, PRODUCT_ID,
        productAndL5MigrationRequest, Constants.DEFAULT_USERNAME, false, false);
    productMigrationWrapperService.migrateProductAndL5DetailsByProductSku(Constants.STORE_ID,
      productAndL5MigrationRequest, Constants.DEFAULT_USERNAME);
    Mockito.verify(productService).migrateProductAndL5Details(Constants.STORE_ID,
      productAndL5MigrationRequest, Constants.DEFAULT_USERNAME);
  }

  @Test
  public void migrateProductAndL5DetailsByProductSkuForDeletedProductTest() throws Exception {
    ProductAndL5MigrationRequest productAndL5MigrationRequest =
      ProductAndL5MigrationRequest.builder().productSku(PRODUCT_SKU)
        .productType(ProductType.BIG_PRODUCT).build();
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setState(ProductLevel1State.DELETED);
    productBusinessPartner.setGdnProductSku(PRODUCT_SKU);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productBusinessPartner.setId(PRODUCT_ID);
    Mockito.when(productBusinessPartnerService.findFirstByProductSku(PRODUCT_SKU)).thenReturn(productBusinessPartner);
    com.gdn.x.product.rest.web.model.request.ProductAndL5MigrationRequest request =
      new com.gdn.x.product.rest.web.model.request.ProductAndL5MigrationRequest();
    BeanUtils.copyProperties(productAndL5MigrationRequest, request);
    Mockito.doNothing().when(productService)
      .migrateProductAndL5DetailsByProductSku(Constants.DEFAULT_STORE_ID, PRODUCT_ID,
        productAndL5MigrationRequest, Constants.DEFAULT_USERNAME, false, false);
    productMigrationWrapperService.migrateProductAndL5DetailsByProductSku(Constants.STORE_ID,
      productAndL5MigrationRequest, Constants.DEFAULT_USERNAME);
    Mockito.verify(productService).migrateProductAndL5Details(Constants.STORE_ID,
      productAndL5MigrationRequest, Constants.DEFAULT_USERNAME);
  }


  @Test
  public void migrateProductAndL5DetailsByProductSkuWithProductSkuNullTest() throws Exception {
    ProductAndL5MigrationRequest productAndL5MigrationRequest =
      ProductAndL5MigrationRequest.builder()
        .productType(ProductType.BIG_PRODUCT).build();
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setState(ProductLevel1State.ACTIVE);
    productBusinessPartner.setGdnProductSku(PRODUCT_SKU);
    productBusinessPartner.setProductId(PRODUCT_ID);
    Mockito.when(productBusinessPartnerService.findFirstByProductSku(PRODUCT_SKU)).thenReturn(productBusinessPartner);
    com.gdn.x.product.rest.web.model.request.ProductAndL5MigrationRequest request =
      new com.gdn.x.product.rest.web.model.request.ProductAndL5MigrationRequest();
    BeanUtils.copyProperties(productAndL5MigrationRequest, request);
    Mockito.doNothing().when(xProductOutbound).migrateProductAndL5DetailByProductSku(request,
      Constants.DEFAULT_STORE_ID, Constants.DEFAULT_USERNAME);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productMigrationWrapperService.migrateProductAndL5DetailsByProductSku(Constants.STORE_ID,
          productAndL5MigrationRequest, Constants.DEFAULT_USERNAME);
    });
  }
}

package com.gdn.mta.product.service;

import com.gda.mta.product.dto.PickupPointUpdateRequest;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.domain.event.modal.ProductFbbMigrationEventModel;
import com.gdn.mta.product.entity.ProductFbbMigration;
import com.gdn.mta.product.enums.FbbMigrationConstants;
import com.gdn.mta.product.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductItemBusinessPartnerRepository;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

public class BackFillFbbFlagServiceImplTest {

  private static final String IDENTIFIER = "identifier";
  private static final String STORE_ID = "store-id";
  private static final String PP_CODE = "pp-code";
  private static final String PRODUCT_SKU = "product-sku";
  private static final String ITEM_SKU = "item-sku";
  private static final String BP_CODE = "pp-code";

  @InjectMocks
  public BackFillFbbFlagServiceImpl backFillFbbFlagService;

  @Mock
  private ProductItemBusinessPartnerRepository productItemBusinessPartnerRepository;

  @Mock
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Mock
  private ProductLevel3Service productLevel3Service;

  private ProductFbbMigrationEventModel productFbbMigration;

  @BeforeEach
  public void setup() {
    MockitoAnnotations.initMocks(this);
    productFbbMigration = new ProductFbbMigrationEventModel();
    productFbbMigration.setIdentifier(IDENTIFIER);
    productFbbMigration.setPickupPointCode(PP_CODE);
    productFbbMigration.setStoreId(STORE_ID);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productLevel3Service);
  }

  @Test
  public void backFillFbbFlagTest() {
    backFillFbbFlagService.backFillFbbFlag(productFbbMigration);
    Mockito.verify(productItemBusinessPartnerRepository)
      .updateFbbFlagByStoreIdAndProductBusinessPartnerIdInAndPickupPointId(
        productFbbMigration.getStoreId(), productFbbMigration.getIdentifier(),
        productFbbMigration.getPickupPointCode(), true);
    Mockito.verify(productBusinessPartnerRepository)
      .updateFbbFlagByStoreIdAndId(productFbbMigration.getStoreId(),
        productFbbMigration.getIdentifier(), true);
  }

  @Test
  public void backFillFbbFlagExceptionTest() {
    Mockito.doThrow(ApplicationRuntimeException.class).when(productItemBusinessPartnerRepository)
      .updateFbbFlagByStoreIdAndProductBusinessPartnerIdInAndPickupPointId(
        productFbbMigration.getStoreId(), productFbbMigration.getIdentifier(),
        productFbbMigration.getPickupPointCode(), true);
    backFillFbbFlagService.backFillFbbFlag(productFbbMigration);
    Mockito.verify(productItemBusinessPartnerRepository)
      .updateFbbFlagByStoreIdAndProductBusinessPartnerIdInAndPickupPointId(
        productFbbMigration.getStoreId(), productFbbMigration.getIdentifier(),
        productFbbMigration.getPickupPointCode(), true);
  }

  @Test
  public void updateFbbPickupPointTest() throws Exception {
    String identifier = PRODUCT_SKU.concat("#").concat(ITEM_SKU).concat("#").concat(BP_CODE);
    productFbbMigration.setIdentifier(identifier);
    productFbbMigration.setProductState(FbbMigrationConstants.ACTIVE.name());
    backFillFbbFlagService.updateFbbPickupPoint(productFbbMigration);
    Mockito.verify(productLevel3Service).updatePickupPointCodes(Mockito.any(PickupPointUpdateRequest.class));
  }

  @Test
  public void updateFbbPickupPointInActiveTest() throws Exception {
    String identifier = PRODUCT_SKU.concat("#").concat(ITEM_SKU).concat("#").concat(BP_CODE);
    productFbbMigration.setIdentifier(identifier);
    productFbbMigration.setProductState(FbbMigrationConstants.INACTIVE.name());
    backFillFbbFlagService.updateFbbPickupPoint(productFbbMigration);
    Mockito.verify(productLevel3Service).updatePickupPointCodes(Mockito.any(PickupPointUpdateRequest.class));
  }
}

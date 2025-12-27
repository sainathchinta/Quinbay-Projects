package com.gdn.mta.product.service.domainevent;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.ArrayList;
import java.util.Arrays;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.modal.ProductMigrationRequestEvent;
import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.service.ProductMigrationWrapperService;
import com.gdn.mta.product.service.ProductSystemParameterService;
import com.gdn.partners.pbp.commons.constants.Constants;

public class ProductMigrationSubscriberTest {

  private static final String PRODUCT_SKU1 = "PRODUCT_SKU1";
  private static final String PRODUCT_SKU2 = "PRODUCT_SKU2";
  private static final String PRODUCT_CODE1 = "PRODUCT_CODE1";
  private static final String PRODUCT_CODE2 = "PRODUCT_CODE2";


  private ObjectMapper mapper;

  @Mock
  private ProductMigrationWrapperService productMigrationWrapperService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductSystemParameterService productSystemParameterService;

  @InjectMocks
  private ProductMigrationSubscriber subscriber;

  private ProductMigrationRequestEvent event;
  private ProductSystemParameter productSystemParameter;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    this.event = new ProductMigrationRequestEvent();
    event.setThreadCount(1);
    event.setProductCodes(Arrays.asList(PRODUCT_CODE1, PRODUCT_CODE2));
    mapper = new ObjectMapper();

    this.productSystemParameter = new ProductSystemParameter();
    productSystemParameter.setValue("true");
  }

  @AfterEach
  public void cleanup(){
    Mockito.verifyNoMoreInteractions(productMigrationWrapperService, objectMapper, productSystemParameterService);
  }

  @Test
  public void onDomainEventConsumedProductCodesTest() throws Exception {
    String message = mapper.writeValueAsString(this.event);
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.PRODUCT_MIGRATION_SWITCH))
        .thenReturn(productSystemParameter);
    Mockito.when(objectMapper.readValue(message, ProductMigrationRequestEvent.class)).thenReturn(this.event);
    Mockito.when(productMigrationWrapperService.migrateProductByProductCode(PRODUCT_CODE1, true)).thenReturn(2);
    Mockito.when(productMigrationWrapperService.migrateProductByProductCode(PRODUCT_CODE2, true)).thenReturn(2);
    this.subscriber.onDomainEventConsumed(message);
    Mockito.verify(productMigrationWrapperService).migrateProductByProductCode(PRODUCT_CODE1, true);
    Mockito.verify(productMigrationWrapperService).migrateProductByProductCode(PRODUCT_CODE2, true);
    verify(objectMapper).readValue(message, ProductMigrationRequestEvent.class);
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.PRODUCT_MIGRATION_SWITCH);
  }

  @Test
  public void onDomainEventConsumedProductCodesSwitchOffTest() throws Exception {
    String message = mapper.writeValueAsString(this.event);
    productSystemParameter.setValue("false");
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.PRODUCT_MIGRATION_SWITCH))
        .thenReturn(productSystemParameter);
    this.subscriber.onDomainEventConsumed(message);
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.PRODUCT_MIGRATION_SWITCH);
  }

  @Test
  public void onDomainEventConsumedProductSkusTest() throws Exception {
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.PRODUCT_MIGRATION_SWITCH))
        .thenReturn(productSystemParameter);
    event.setProductCodes(null);
    event.setProductSkuList(Arrays.asList(PRODUCT_SKU1, PRODUCT_SKU2));
    String message = mapper.writeValueAsString(this.event);
    Mockito.when(objectMapper.readValue(message, ProductMigrationRequestEvent.class)).thenReturn(this.event);
    Mockito.when(productMigrationWrapperService.migrateProductByProductSkus(Arrays.asList(PRODUCT_SKU1, PRODUCT_SKU2)))
        .thenReturn(2);
    this.subscriber.onDomainEventConsumed(message);
    Mockito.verify(productMigrationWrapperService)
        .migrateProductByProductSkus(Arrays.asList(PRODUCT_SKU1, PRODUCT_SKU2));
    verify(objectMapper).readValue(message, ProductMigrationRequestEvent.class);
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.PRODUCT_MIGRATION_SWITCH);
  }

  @Test
  public void onDomainEventConsumedProductSkusExceptionTest() throws Exception {
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.PRODUCT_MIGRATION_SWITCH))
        .thenReturn(productSystemParameter);
    event.setProductCodes(null);
    event.setProductSkuList(Arrays.asList(PRODUCT_SKU1, PRODUCT_SKU2));
    String message = mapper.writeValueAsString(this.event);
    Mockito.when(objectMapper.readValue(message, ProductMigrationRequestEvent.class)).thenReturn(this.event);
    Mockito.doThrow(Exception.class).when(productMigrationWrapperService)
        .migrateProductByProductSkus(Arrays.asList(PRODUCT_SKU1, PRODUCT_SKU2));
    this.subscriber.onDomainEventConsumed(message);
    Mockito.verify(productMigrationWrapperService)
        .migrateProductByProductSkus(Arrays.asList(PRODUCT_SKU1, PRODUCT_SKU2));
    verify(objectMapper).readValue(message, ProductMigrationRequestEvent.class);
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.PRODUCT_MIGRATION_SWITCH);
  }


  @Test
  public void onDomainEventConsumedPBothListEmptyest() throws Exception {
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.PRODUCT_MIGRATION_SWITCH))
        .thenReturn(productSystemParameter);
    event.setProductCodes(null);
    event.setProductSkuList(null);
    String message = mapper.writeValueAsString(this.event);
    this.subscriber.onDomainEventConsumed(message);
    Mockito.verify(productMigrationWrapperService, times(0)).migrateProductByProductSkus(Mockito.anyList());
    Mockito.verify(productMigrationWrapperService, times(0))
        .migrateProductByProductCode(Mockito.anyString(), Mockito.eq(true));
    verify(objectMapper).readValue(message, ProductMigrationRequestEvent.class);
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.PRODUCT_MIGRATION_SWITCH);
  }

  @Test
  public void onDomainEventConsumedProductCodes_emptyProductCodesTest() throws Exception {
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.PRODUCT_MIGRATION_SWITCH))
        .thenReturn(productSystemParameter);
    event.setProductCodes(new ArrayList<>());
    String message = mapper.writeValueAsString(this.event);
    Mockito.when(objectMapper.readValue(message, ProductMigrationRequestEvent.class))
        .thenReturn(this.event);
    this.subscriber.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, ProductMigrationRequestEvent.class);
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.PRODUCT_MIGRATION_SWITCH);
  }

  @Test
  public void onDomainEventConsumed2ThreadProductCodesTest() throws Exception {
    event.setThreadCount(2);
    String message = mapper.writeValueAsString(this.event);
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.PRODUCT_MIGRATION_SWITCH))
        .thenReturn(productSystemParameter);
    Mockito.when(objectMapper.readValue(message, ProductMigrationRequestEvent.class)).thenReturn(this.event);
    Mockito.when(productMigrationWrapperService.migrateProductByProductCode(PRODUCT_CODE1, true)).thenReturn(2);
    Mockito.when(productMigrationWrapperService.migrateProductByProductCode(PRODUCT_CODE2, true)).thenReturn(2);
    this.subscriber.onDomainEventConsumed(message);
    Mockito.verify(productMigrationWrapperService).migrateProductByProductCode(PRODUCT_CODE1, true);
    Mockito.verify(productMigrationWrapperService).migrateProductByProductCode(PRODUCT_CODE2, true);
    verify(objectMapper).readValue(message, ProductMigrationRequestEvent.class);
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.PRODUCT_MIGRATION_SWITCH);
  }

  @Test
  public void onDomainEventConsumed2ThreadsProductSkusTest() throws Exception {
    event.setThreadCount(2);
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.PRODUCT_MIGRATION_SWITCH))
        .thenReturn(productSystemParameter);
    event.setProductCodes(null);
    event.setProductSkuList(Arrays.asList(PRODUCT_SKU1, PRODUCT_SKU2));

    String message = mapper.writeValueAsString(this.event);
    Mockito.when(objectMapper.readValue(message, ProductMigrationRequestEvent.class)).thenReturn(this.event);
    Mockito.when(productMigrationWrapperService.migrateProductByProductSkus(Mockito.anyList())).thenReturn(2);
    this.subscriber.onDomainEventConsumed(message);
    Mockito.verify(productMigrationWrapperService, times(2)).migrateProductByProductSkus(Mockito.anyList());
    verify(objectMapper).readValue(message, ProductMigrationRequestEvent.class);
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.PRODUCT_MIGRATION_SWITCH);
  }
}

package com.gdn.x.product.service.event.listener;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.merchant.voucher.streaming.model.VoucherItemSkusEventModel;
import com.gdn.x.product.service.api.ItemService;

public class MerchantVoucherProductEventListenerTest {

  private static final String MERCHANT_CODE = "merchantCode";
  private static final String STORE_ID = "storeId";
  private static final String VOUCHER_CODE = "voucherCode";
  private static final String MESSAGE = "message";

  @Mock
  private ItemService itemService;

  @Mock
  private ObjectMapper objectMapper;

  @InjectMocks
  private MerchantVoucherProductEventListener merchantVoucherProductEventListener;

  @Captor
  private ArgumentCaptor<VoucherItemSkusEventModel> voucherItemSkusEventModelArgumentCaptor;

  private VoucherItemSkusEventModel voucherItemSkusEventModel;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.openMocks(this);
    doNothing().when(itemService).publishItemsByMerchantCodeToVoucher(any(VoucherItemSkusEventModel.class));
    voucherItemSkusEventModel = VoucherItemSkusEventModel.builder().merchantCode(MERCHANT_CODE)
        .storeId(STORE_ID).voucherCode(VOUCHER_CODE).build();
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(itemService);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, VoucherItemSkusEventModel.class))
      .thenReturn(voucherItemSkusEventModel);
    merchantVoucherProductEventListener.onDomainEventConsumed(MESSAGE);
    verify(itemService).publishItemsByMerchantCodeToVoucher(voucherItemSkusEventModelArgumentCaptor.capture());
    verify(this.objectMapper).readValue(MESSAGE, VoucherItemSkusEventModel.class);
    assertNotNull(voucherItemSkusEventModelArgumentCaptor.getValue().getVoucherCreatedDate());
    assertEquals(voucherItemSkusEventModelArgumentCaptor.getValue().getVoucherCode(), VOUCHER_CODE);
  }

  @Test
  public void onDomainEventConsumedTest_WhenStoreIdIsNull() throws Exception {
    voucherItemSkusEventModel = VoucherItemSkusEventModel.builder().build();
    Mockito.when(this.objectMapper.readValue(MESSAGE, VoucherItemSkusEventModel.class))
      .thenReturn(voucherItemSkusEventModel);
    merchantVoucherProductEventListener.onDomainEventConsumed(MESSAGE);
    verify(this.objectMapper).readValue(MESSAGE, VoucherItemSkusEventModel.class);
  }

  @Test
  public void onDomainEventConsumedTest_WhenMerhcantCodeIsNull() throws Exception {
    voucherItemSkusEventModel = VoucherItemSkusEventModel.builder().storeId(STORE_ID).build();
    Mockito.when(this.objectMapper.readValue(MESSAGE, VoucherItemSkusEventModel.class))
      .thenReturn(voucherItemSkusEventModel);
    merchantVoucherProductEventListener.onDomainEventConsumed(MESSAGE);
    verify(this.objectMapper).readValue(MESSAGE, VoucherItemSkusEventModel.class);
  }

  @Test
  public void onDomainEventConsumedTest_WhenVoucherCodeIsNull() throws Exception {
    voucherItemSkusEventModel = VoucherItemSkusEventModel.builder().storeId(STORE_ID)
        .merchantCode(MERCHANT_CODE).build();
    Mockito.when(this.objectMapper.readValue(MESSAGE, VoucherItemSkusEventModel.class))
      .thenReturn(voucherItemSkusEventModel);
    merchantVoucherProductEventListener.onDomainEventConsumed(MESSAGE);
    verify(this.objectMapper).readValue(MESSAGE, VoucherItemSkusEventModel.class);
  }
}
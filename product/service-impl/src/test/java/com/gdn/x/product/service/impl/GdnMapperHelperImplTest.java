package com.gdn.x.product.service.impl;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.modelmapper.ModelMapper;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.mapper.GdnMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.model.vo.ProductForTransactionVO;
import com.gdn.x.product.rest.web.model.response.ProductForTransactionResponse;

public class GdnMapperHelperImplTest {

  private static final String ITEMSKU = "itemsku";

  private static final String STORE_ID = "store-id";

  private ProductForTransactionVO product;

  private ProductForTransactionResponse productResponseRestWeb;

  @InjectMocks
  private GdnMapperHelperImpl gdnMapperHelperImpl;

  @Mock
  private ModelMapper modelMapper;

  @Mock
  private ObjectMapper objectMapper;

  @Test
  public void mapBeanNotSuccess() {
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> this.gdnMapperHelperImpl.mapBean(null, ProductForTransactionResponse.class));
  }

  @Test
  public void mapBeanSuccess() {
    ProductForTransactionResponse result =
        this.gdnMapperHelperImpl.mapBean(this.product, ProductForTransactionResponse.class);
    verify(this.modelMapper).map(this.product, ProductForTransactionResponse.class);
    assertThat(result, notNullValue());
    assertThat(result, equalTo(this.productResponseRestWeb));
  }

  @Test
  public void deepCopyObjectMapperTest() {
    ReflectionTestUtils.setField(gdnMapperHelperImpl, "useModelMapperBean", false);
    ProductForTransactionResponse result =
        this.gdnMapperHelperImpl.deepCopy(this.product, ProductForTransactionResponse.class);
    verify(objectMapper).convertValue(this.product, ProductForTransactionResponse.class);
    assertThat(result, notNullValue());
    assertThat(result, equalTo(this.productResponseRestWeb));
  }

  @Test
  public void deepCopyNullTest() {
    ReflectionTestUtils.setField(gdnMapperHelperImpl, "useModelMapperBean", false);
    ProductForTransactionResponse result =
        this.gdnMapperHelperImpl.deepCopy(null, ProductForTransactionResponse.class);
    Assertions.assertNull(result);
  }

  @BeforeEach
  public void setup() {
    openMocks(this);
    this.product = new ProductForTransactionVO();
    this.product.setItemSku(GdnMapperHelperImplTest.ITEMSKU);

    this.productResponseRestWeb = new ProductForTransactionResponse();
    this.productResponseRestWeb.setStoreId(GdnMapperHelperImplTest.STORE_ID);

    when(this.modelMapper.map(this.product, ProductForTransactionResponse.class)).thenReturn(
        this.productResponseRestWeb);

    when(this.objectMapper.convertValue(this.product, ProductForTransactionResponse.class)).thenReturn(
        this.productResponseRestWeb);

    ReflectionTestUtils.setField(gdnMapperHelperImpl, "useModelMapperBean", true);
  }

  @AfterEach
  public void teardown() {
    verifyNoMoreInteractions(this.modelMapper);
  }
}

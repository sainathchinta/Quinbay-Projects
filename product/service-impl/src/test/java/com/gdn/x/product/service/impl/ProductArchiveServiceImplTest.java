package com.gdn.x.product.service.impl;

import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.dao.api.ProductArchiveRepository;
import com.gdn.x.product.model.entity.ProductArchive;

public class ProductArchiveServiceImplTest {

  @InjectMocks
  private ProductArchiveServiceImpl productArchiveService;

  @Mock
  private ProductArchiveRepository productArchiveRepository;

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
  }

  @Test
  public void addProductsToProductArchiveTest() throws Exception {
    List<ProductArchive> productArchiveList = new ArrayList<>();
    productArchiveList.add(new ProductArchive());
    productArchiveService.addProductsToProductArchive(productArchiveList);
    Mockito.verify(productArchiveRepository).saveAll(productArchiveList);
  }

  @Test
  public void addProductsToProductArchiveExceptionTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> productArchiveService.addProductsToProductArchive(new ArrayList<>()));
  }
}

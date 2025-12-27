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
import com.gdn.x.product.dao.api.ItemArchiveRepository;
import com.gdn.x.product.model.entity.ItemArchive;

public class ItemArchiveServiceImplTest {

  @InjectMocks
  private ItemArchiveServiceImpl itemArchiveService;

  @Mock
  private ItemArchiveRepository itemArchiveRepository;

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
  }

  @Test
  public void addItemsToItemArchiveTest() throws Exception {
    List<ItemArchive> itemArchiveList = new ArrayList<>();
    itemArchiveList.add(new ItemArchive());
    itemArchiveService.addItemsToItemArchive(itemArchiveList);
    Mockito.verify(itemArchiveRepository).saveAll(itemArchiveList);
  }

  @Test
  public void addItemsToItemArchiveExceptionTest() throws Exception {
    List<ItemArchive> itemArchiveList = new ArrayList<>();
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  itemArchiveService.addItemsToItemArchive(itemArchiveList));
  }
}

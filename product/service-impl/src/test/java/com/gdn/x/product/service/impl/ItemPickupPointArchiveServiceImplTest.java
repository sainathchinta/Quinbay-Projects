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
import com.gdn.x.product.dao.api.ItemPickupPointArchiveRepository;
import com.gdn.x.product.model.entity.ItemPickupPointArchive;

public class ItemPickupPointArchiveServiceImplTest {

  @InjectMocks
  private ItemPickupPointArchiveServiceImpl itemPickupPointArchiveService;

  @Mock
  private ItemPickupPointArchiveRepository itemPickupPointArchiveRepository;

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
  }

  @Test
  public void addItemPickupPointsToItemPickupPointArchiveTest() throws Exception {
    List<ItemPickupPointArchive> itemPickupPointArchiveList = new ArrayList<>();
    itemPickupPointArchiveList.add(new ItemPickupPointArchive());
    Mockito.when(itemPickupPointArchiveRepository.saveAll(itemPickupPointArchiveList)).thenReturn(new ArrayList<>());
    itemPickupPointArchiveService.addItemPickupPointsToItemPickupPointArchive(itemPickupPointArchiveList);
    Mockito.verify(itemPickupPointArchiveRepository).saveAll(itemPickupPointArchiveList);
  }

  @Test
  public void addItemPickupPointsToItemPickupPointArchiveExceptionTest() throws Exception {
    List<ItemPickupPointArchive> itemPickupPointArchiveList = new ArrayList<>();
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  itemPickupPointArchiveService.addItemPickupPointsToItemPickupPointArchive(itemPickupPointArchiveList));
  }
}

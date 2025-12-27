package com.gdn.x.product.service.util;

import static org.mockito.MockitoAnnotations.openMocks;

import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.model.entity.Item;

public class ListUtilImplTest {

  @InjectMocks
  private ListUtilImpl listUtilImpl;

  @Test
  public void distinctTest() {
    Item item1 = new Item("item-1", "produk-1");
    Item item2 = new Item("item-1", "produk-2");
    Item item3 = new Item("item-2", "produk-1");
    List<Item> items = Arrays.asList(item1, item2, item3);
    List<Item> expected = Arrays.asList(item1, item3);
    List<Item> distinctedItem = this.listUtilImpl.distinct(items, Item::getItemSku);
    Assertions.assertEquals(distinctedItem, expected);
  }

  @Test
  public void distinctTestWithNullList() {
    List<Item> items = null;
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> this.listUtilImpl.distinct(items, Item::getItemSku));
  }

  @BeforeEach
  public void init() {
    openMocks(this);
  }
}

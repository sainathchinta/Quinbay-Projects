package com.gdn.partners.pbp.util;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.gdn.x.product.domain.event.model.ItemViewConfig;
import com.gdn.x.product.domain.event.model.Price;
import com.gdn.x.product.enums.ChannelName;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;

public class ProductLevel3UtilTest {

  @Test
  public void getItemViewConfigDTO_SearchEmptyList_Test() {
    ProductLevel3Util.getItemViewConfigDTO(new ArrayList<>(), ChannelName.DEFAULT);
  }

  @Test
  public void getItemViewConfigDTO_Found_Test() {
    List<ItemViewConfigDTO> searchItems = new ArrayList<>();
    ItemViewConfigDTO searchItem = new ItemViewConfigDTO();
    searchItem.setChannel(ChannelName.DEFAULT.name());
    searchItems.add(searchItem);
    Assertions.assertNotNull(ProductLevel3Util.getItemViewConfigDTO(searchItems, ChannelName.DEFAULT));
  }

  @Test
  public void getItemViewConfigDTO_NotFound_Test() {
    List<ItemViewConfigDTO> searchItems = new ArrayList<>();
    ItemViewConfigDTO searchItem = new ItemViewConfigDTO();
    searchItem.setChannel(ChannelName.MOBILE_WEB.name());
    searchItems.add(searchItem);
    Assertions.assertNull(ProductLevel3Util.getItemViewConfigDTO(searchItems, ChannelName.DEFAULT));
  }

  @Test
  public void getItemViewConfig_SearchEmptyList_Test() {
    ProductLevel3Util.getItemViewConfig(new ArrayList<>(), ChannelName.DEFAULT);
  }

  @Test
  public void getItemViewConfig_Found_Test() {
    List<ItemViewConfig> searchItems = new ArrayList<>();
    ItemViewConfig searchItem = new ItemViewConfig();
    searchItem.setChannel(ChannelName.DEFAULT.name());
    searchItems.add(searchItem);
    Assertions.assertNotNull(ProductLevel3Util.getItemViewConfig(searchItems, ChannelName.DEFAULT));
  }

  @Test
  public void getItemViewConfig_NotFound_Test() {
    List<ItemViewConfig> searchItems = new ArrayList<>();
    ItemViewConfig searchItem = new ItemViewConfig();
    searchItem.setChannel(ChannelName.MOBILE_WEB.name());
    searchItems.add(searchItem);
    Assertions.assertNull(ProductLevel3Util.getItemViewConfig(searchItems, ChannelName.DEFAULT));
  }

  @Test
  public void getItemPrice_SearchEmptyList_Test() {
    ProductLevel3Util.getItemPrice(new ArrayList<>(), ChannelName.DEFAULT);
  }

  @Test
  public void getItemPrice_Found_Test() {
    List<Price> searchItems = new ArrayList<>();
    Price searchItem = new Price();
    searchItem.setChannel(ChannelName.DEFAULT.name());
    searchItems.add(searchItem);
    Assertions.assertNotNull(ProductLevel3Util.getItemPrice(searchItems, ChannelName.DEFAULT));
  }

  @Test
  public void getItemPrice_NotFound_Test() {
    List<Price> searchItems = new ArrayList<>();
    Price searchItem = new Price();
    searchItem.setChannel(ChannelName.MOBILE_WEB.name());
    searchItems.add(searchItem);
    Assertions.assertNull(ProductLevel3Util.getItemPrice(searchItems, ChannelName.DEFAULT));
  }

  @Test
  public void privateConstructor_Test() throws Exception {
    Assertions.assertTrue(Modifier.isFinal(ProductLevel3Util.class.getModifiers()), "class must be final");
    final Constructor<ProductLevel3Util> constructor =
        ProductLevel3Util.class.getDeclaredConstructor();
    if (constructor.isAccessible() || !Modifier.isPrivate(constructor.getModifiers())) {
      Assertions.fail("constructor is not private");
    }
    constructor.setAccessible(true);
    constructor.newInstance();
    constructor.setAccessible(false);
    for (final Method method : ProductLevel3Util.class.getMethods()) {
      if (!Modifier.isStatic(method.getModifiers())
          && method.getDeclaringClass().equals(ProductLevel3Util.class)) {
        Assertions.fail("there exists a non-static method:" + method);
      }
    }
  }
}

package com.gdn.x.productcategorybase.service;

public interface MapperService {
   <T> T mapBean(Object source, Class<T> destination);
}

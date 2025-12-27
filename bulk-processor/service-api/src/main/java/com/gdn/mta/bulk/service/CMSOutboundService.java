package com.gdn.mta.bulk.service;

public interface CMSOutboundService {

  String generateShortUrl();

  Boolean mapLongAndShortUrl(String shortUrl, String longUrl);
}

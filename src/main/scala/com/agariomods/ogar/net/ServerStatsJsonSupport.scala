package com.agariomods.ogar.net

import com.agariomods.ogar.ServerStats
import spray.httpx.SprayJsonSupport
import spray.json.DefaultJsonProtocol

object ServerStatsJsonSupport extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val ServerStatsFormat = jsonFormat6(ServerStats)
}

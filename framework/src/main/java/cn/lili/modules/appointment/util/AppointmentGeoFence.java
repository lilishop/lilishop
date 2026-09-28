package cn.lili.modules.appointment.util;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONArray;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;

import java.util.ArrayList;
import java.util.List;

/**
 * 上门服务电子围栏。坐标为腾讯地图 GCJ-02，path 为 [经度, 纬度]。
 */
public final class AppointmentGeoFence {

    private AppointmentGeoFence() {
    }

    /**
     * 解析全部围栏。兼容旧数据 {"path":[[lng,lat],...]} 和新数据 {"paths":[...]}。
     */
    public static List<List<double[]>> parsePolygons(String raw) {
        if (CharSequenceUtil.isBlank(raw) || !JSONUtil.isTypeJSON(raw)) {
            return List.of();
        }
        JSONObject json = JSONUtil.parseObj(raw);
        List<List<double[]>> polygons = new ArrayList<>();
        JSONArray paths = json.getJSONArray("paths");
        if (paths != null) {
            for (int i = 0; i < paths.size(); i++) {
                List<double[]> ring = readRing(paths.getJSONArray(i));
                if (ring.size() >= 3) {
                    polygons.add(ring);
                }
            }
        }
        if (!polygons.isEmpty()) {
            return polygons;
        }
        List<double[]> single = readRing(json.getJSONArray("path"));
        if (single.size() >= 3) {
            polygons.add(single);
        }
        return polygons;
    }

    /**
     * 解析第一块围栏顶点。少于 3 个点时返回空列表。
     */
    public static List<double[]> parsePath(String raw) {
        List<List<double[]>> polygons = parsePolygons(raw);
        return polygons.isEmpty() ? List.of() : polygons.get(0);
    }

    /**
     * 规范成 {"paths":[[[lng,lat],...],...]}。没有任何有效围栏时返回 null。
     */
    public static String normalize(String raw) {
        List<List<double[]>> polygons = parsePolygons(raw);
        if (polygons.isEmpty()) {
            return null;
        }
        JSONArray paths = new JSONArray();
        for (List<double[]> polygon : polygons) {
            JSONArray path = new JSONArray();
            for (double[] point : polygon) {
                JSONArray item = new JSONArray();
                item.add(round6(point[0]));
                item.add(round6(point[1]));
                path.add(item);
            }
            paths.add(path);
        }
        JSONObject json = new JSONObject();
        json.set("paths", paths);
        return json.toString();
    }

    /**
     * 点落在任意一块围栏内即视为在上门范围内。
     */
    public static boolean containsAny(double lng, double lat, List<List<double[]>> polygons) {
        if (polygons == null) {
            return false;
        }
        for (List<double[]> path : polygons) {
            if (contains(lng, lat, path)) {
                return true;
            }
        }
        return false;
    }

    private static List<double[]> readRing(JSONArray path) {
        if (path == null || path.size() < 3) {
            return List.of();
        }
        List<double[]> points = new ArrayList<>();
        for (int i = 0; i < path.size(); i++) {
            JSONArray point = path.getJSONArray(i);
            if (point == null || point.size() < 2) {
                continue;
            }
            Double lng = point.getDouble(0);
            Double lat = point.getDouble(1);
            if (!valid(lng, lat)) {
                continue;
            }
            if (!points.isEmpty()) {
                double[] prev = points.get(points.size() - 1);
                if (same(prev[0], lng) && same(prev[1], lat)) {
                    continue;
                }
            }
            points.add(new double[]{lng, lat});
        }
        if (points.size() >= 2) {
            double[] first = points.get(0);
            double[] last = points.get(points.size() - 1);
            if (same(first[0], last[0]) && same(first[1], last[1])) {
                points.remove(points.size() - 1);
            }
        }
        return points.size() >= 3 ? points : List.of();
    }

    /**
     * 射线法判断点是否在多边形内。
     */
    public static boolean contains(double lng, double lat, List<double[]> path) {
        if (path == null || path.size() < 3 || !valid(lng, lat)) {
            return false;
        }
        boolean inside = false;
        int size = path.size();
        for (int i = 0, j = size - 1; i < size; j = i++) {
            double xi = path.get(i)[0];
            double yi = path.get(i)[1];
            double xj = path.get(j)[0];
            double yj = path.get(j)[1];
            boolean cross = (yi > lat) != (yj > lat)
                    && lng < (xj - xi) * (lat - yi) / (yj - yi) + xi;
            if (cross) {
                inside = !inside;
            }
        }
        return inside;
    }

    private static boolean valid(Double lng, Double lat) {
        return lng != null && lat != null
                && lng >= -180 && lng <= 180
                && lat >= -90 && lat <= 90;
    }

    private static boolean same(double a, double b) {
        return Math.abs(a - b) < 0.000001D;
    }

    private static double round6(double value) {
        return Math.round(value * 1_000_000D) / 1_000_000D;
    }
}

--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: events; Type: SCHEMA; Schema: -; Owner: p-rout
--

CREATE SCHEMA events;


ALTER SCHEMA events OWNER TO "p-rout";

--
-- Name: logs; Type: SCHEMA; Schema: -; Owner: p-rout
--

CREATE SCHEMA logs;


ALTER SCHEMA logs OWNER TO "p-rout";

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = events, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: event; Type: TABLE; Schema: events; Owner: p-rout; Tablespace: 
--

CREATE TABLE event (
    p_rout_id integer,
    data text
);


ALTER TABLE events.event OWNER TO "p-rout";

--
-- Name: header; Type: TABLE; Schema: events; Owner: p-rout; Tablespace: 
--

CREATE TABLE header (
    p_rout_id integer,
    powerrouter_id text,
    time_send text,
    ver text,
    verification text
);


ALTER TABLE events.header OWNER TO "p-rout";

SET search_path = logs, pg_catalog;

--
-- Name: header; Type: TABLE; Schema: logs; Owner: p-rout; Tablespace: 
--

CREATE TABLE header (
    p_rout_id integer,
    period text,
    powerrouter_id text,
    time_send text,
    version text
);


ALTER TABLE logs.header OWNER TO "p-rout";

--
-- Name: module_statuses; Type: TABLE; Schema: logs; Owner: p-rout; Tablespace: 
--

CREATE TABLE module_statuses (
    p_rout_id integer,
    module_id text,
    param_0 text,
    param_1 text,
    param_10 text,
    param_11 text,
    param_12 text,
    param_2 text,
    param_3 text,
    param_4 text,
    param_5 text,
    param_6 text,
    param_7 text,
    param_8 text,
    param_9 text,
    status text,
    version text
);


ALTER TABLE logs.module_statuses OWNER TO "p-rout";

SET search_path = events, pg_catalog;

--
-- Data for Name: event; Type: TABLE DATA; Schema: events; Owner: p-rout
--

INSERT INTO event (p_rout_id, data) VALUES (1, 'D6305E0C880028000000000000000000A3FFFFFF9F0AF7FF00000000000000001E0026192927000000000000');
INSERT INTO event (p_rout_id, data) VALUES (2, 'D6305E0C8800210000000000000000006FF1D3009F0AF7FF00000000000000001E0026192927000000000000');
INSERT INTO event (p_rout_id, data) VALUES (3, 'D7305E0C880022000000000000000000FB2DF9FF9F0AF7FF00000000000000001E0026192927000000000000');


--
-- Data for Name: header; Type: TABLE DATA; Schema: events; Owner: p-rout
--

INSERT INTO header (p_rout_id, powerrouter_id, time_send, ver, verification) VALUES (1, 'POWERROUTERIDXXX', '2014-07-29T16:13:45+01:00', '1', '0');
INSERT INTO header (p_rout_id, powerrouter_id, time_send, ver, verification) VALUES (2, 'POWERROUTERIDXXX', '2014-07-29T16:13:47+01:00', '1', '0');
INSERT INTO header (p_rout_id, powerrouter_id, time_send, ver, verification) VALUES (3, 'POWERROUTERIDXXX', '2014-07-29T16:13:47+01:00', '1', '0');


SET search_path = logs, pg_catalog;

--
-- Data for Name: header; Type: TABLE DATA; Schema: logs; Owner: p-rout
--

INSERT INTO header (p_rout_id, period, powerrouter_id, time_send, version) VALUES (1, '60', 'POWERROUTERIDXXX', '2014-07-29T16:14:01+01:00', '3');
INSERT INTO header (p_rout_id, period, powerrouter_id, time_send, version) VALUES (2, '60', 'POWERROUTERIDXXX', '2014-07-29T16:15:01+01:00', '3');
INSERT INTO header (p_rout_id, period, powerrouter_id, time_send, version) VALUES (3, '60', 'POWERROUTERIDXXX', '2014-07-29T16:16:01+01:00', '3');
INSERT INTO header (p_rout_id, period, powerrouter_id, time_send, version) VALUES (4, '60', 'POWERROUTERIDXXX', '2014-07-29T16:17:01+01:00', '3');
INSERT INTO header (p_rout_id, period, powerrouter_id, time_send, version) VALUES (5, '60', 'POWERROUTERIDXXX', '2014-07-29T16:18:01+01:00', '3');
INSERT INTO header (p_rout_id, period, powerrouter_id, time_send, version) VALUES (6, '60', 'POWERROUTERIDXXX', '2014-07-29T16:19:01+01:00', '3');
INSERT INTO header (p_rout_id, period, powerrouter_id, time_send, version) VALUES (7, '60', 'POWERROUTERIDXXX', '2014-07-29T16:20:01+01:00', '3');
INSERT INTO header (p_rout_id, period, powerrouter_id, time_send, version) VALUES (8, '60', 'POWERROUTERIDXXX', '2014-07-29T16:21:01+01:00', '3');
INSERT INTO header (p_rout_id, period, powerrouter_id, time_send, version) VALUES (9, '60', 'POWERROUTERIDXXX', '2014-07-29T16:22:01+01:00', '3');
INSERT INTO header (p_rout_id, period, powerrouter_id, time_send, version) VALUES (10, '60', 'POWERROUTERIDXXX', '2014-07-29T16:23:01+01:00', '3');


--
-- Data for Name: module_statuses; Type: TABLE DATA; Schema: logs; Owner: p-rout
--

INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (1, '16', '4997', '2367', NULL, NULL, NULL, '400', '-1935', '1247552', '2032120', NULL, NULL, NULL, NULL, '18691', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (1, '9', '4998', '2362', '547', NULL, NULL, '-2285', '2868821', '9099', '2349', '0', '2729', '40937', '-2268', '16147', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (1, '136', '2720', '-99', '15500', '2412', '0', '-29', '531391', '630187', '100', '240', '302', '498', '2720', '19779', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (1, '12', '29825', '619', '2533', '3362934', NULL, '1849', '1421522', '560', '30159', '227', '685', '1941412', '520', '49219', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (1, '11', '2293', '42', '115', '1592000', NULL, '27', '428200', '2348', '886', '-2077', '1078300', '2324', '85', '1555', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (2, '16', '4998', '2367', NULL, NULL, NULL, '400', '-1911', '1247585', '2032120', NULL, NULL, NULL, NULL, '18691', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (2, '9', '4998', '2363', '547', NULL, NULL, '-2280', '2868859', '9099', '2350', '0', '2729', '40935', '-2263', '16147', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (2, '136', '2720', '-97', '15500', '2412', '0', '-28', '531391', '630188', '100', '240', '302', '498', '2720', '19779', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (2, '12', '29924', '619', '2525', '3362976', NULL, '1846', '1421553', '570', '30104', '225', '679', '1941423', '520', '49219', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (2, '11', '2296', '44', '115', '1592000', NULL, '30', '428200', '2348', '878', '-2056', '1078300', '2320', '84', '1555', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (3, '16', '4998', '2365', NULL, NULL, NULL, '400', '-1916', '1247617', '2032120', NULL, NULL, NULL, NULL, '18691', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (3, '9', '4998', '2361', '547', NULL, NULL, '-2273', '2868897', '9099', '2347', '0', '2729', '40922', '-2257', '16147', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (3, '136', '2720', '-95', '15500', '2412', '0', '-24', '531391', '630188', '100', '240', '302', '498', '2720', '19779', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (3, '12', '29825', '618', '2515', '3363018', NULL, '1844', '1421584', '570', '29904', '224', '670', '1941434', '520', '49219', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (3, '11', '2294', '44', '114', '1592000', NULL, '30', '428200', '2343', '881', '-2060', '1078300', '2323', '85', '1555', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (4, '16', '4998', '2368', NULL, NULL, NULL, '400', '-1919', '1247649', '2032120', NULL, NULL, NULL, NULL, '18691', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (4, '9', '4997', '2363', '546', NULL, NULL, '-2261', '2868935', '9099', '2350', '0', '2729', '40934', '-2246', '16147', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (4, '136', '2719', '-99', '15500', '2412', '0', '-28', '531391', '630189', '100', '240', '302', '498', '2720', '19779', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (4, '12', '29174', '632', '2507', '3363060', NULL, '1843', '1421614', '570', '29907', '222', '664', '1941446', '520', '49219', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (4, '11', '2298', '42', '111', '1592000', NULL, '25', '428200', '2348', '877', '-2055', '1078300', '2320', '85', '1555', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (5, '16', '4999', '2358', NULL, NULL, NULL, '400', '-1711', '1247681', '2032120', NULL, NULL, NULL, NULL, '18691', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (5, '9', '4999', '2356', '547', NULL, NULL, '-2264', '2868973', '9099', '2343', '0', '2729', '40945', '-2248', '16147', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (5, '136', '2719', '-95', '15500', '2412', '0', '-30', '531391', '630189', '100', '240', '302', '498', '2720', '19779', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (5, '12', '29074', '636', '2511', '3363101', NULL, '1851', '1421645', '570', '30154', '218', '659', '1941456', '510', '49219', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (5, '11', '2306', '43', '111', '1592000', NULL, '27', '428200', '2339', '812', '-1849', '1078300', '2320', '84', '1555', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (6, '16', '4999', '2362', NULL, NULL, NULL, '400', '-1753', '1247710', '2032120', NULL, NULL, NULL, NULL, '18691', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (6, '9', '4999', '2358', '547', NULL, NULL, '-2285', '2869010', '9099', '2345', '0', '2729', '40932', '-2269', '16147', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (6, '136', '2722', '-98', '15500', '2412', '0', '-28', '531391', '630190', '100', '240', '302', '498', '2720', '19779', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (6, '12', '29178', '642', '2530', '3363142', NULL, '1875', '1421675', '570', '30153', '217', '655', '1941467', '510', '49219', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (6, '11', '2307', '43', '116', '1592000', NULL, '28', '428200', '2343', '823', '-1897', '1078300', '2319', '85', '1555', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (7, '16', '4998', '2364', NULL, NULL, NULL, '400', '-1799', '1247739', '2032120', NULL, NULL, NULL, NULL, '18691', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (7, '9', '4999', '2360', '547', NULL, NULL, '-2323', '2869048', '9099', '2346', '0', '2729', '40917', '-2307', '16147', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (7, '136', '2719', '-100', '15500', '2412', '0', '-23', '531391', '630190', '100', '240', '302', '498', '2720', '19779', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (7, '12', '29124', '659', '2573', '3363185', NULL, '1919', '1421707', '570', '30304', '216', '653', '1941478', '510', '49219', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (7, '11', '2311', '43', '109', '1592000', NULL, '29', '428200', '2343', '841', '-1937', '1078300', '2321', '80', '1555', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (8, '16', '4998', '2364', NULL, NULL, NULL, '400', '-1789', '1247769', '2032120', NULL, NULL, NULL, NULL, '18691', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (8, '9', '4998', '2360', '548', NULL, NULL, '-2314', '2869087', '9099', '2347', '0', '2729', '40931', '-2299', '16147', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (8, '136', '2720', '-95', '15500', '2412', '0', '-21', '531391', '630191', '100', '240', '302', '499', '2720', '19779', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (8, '12', '29224', '658', '2560', '3363229', NULL, '1917', '1421740', '570', '29903', '215', '644', '1941489', '510', '49219', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (8, '11', '2313', '44', '114', '1592000', NULL, '29', '428200', '2342', '829', '-1932', '1078300', '2325', '85', '1555', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (9, '16', '4999', '2361', NULL, NULL, NULL, '400', '-1833', '1247799', '2032120', NULL, NULL, NULL, NULL, '18691', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (9, '9', '4999', '2357', '549', NULL, NULL, '-2333', '2869126', '9099', '2344', '0', '2729', '40934', '-2318', '16147', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (9, '136', '2719', '-98', '15500', '2412', '0', '-22', '531391', '630191', '100', '240', '302', '499', '2720', '19779', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (9, '12', '29228', '663', '2578', '3363272', NULL, '1939', '1421772', '570', '30104', '212', '639', '1941500', '510', '49219', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (9, '11', '2310', '42', '106', '1592000', NULL, '25', '428200', '2342', '854', '-1964', '1078300', '2330', '81', '1555', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (10, '16', '4997', '2360', NULL, NULL, NULL, '400', '-1818', '1247829', '2032120', NULL, NULL, NULL, NULL, '18691', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (10, '9', '4997', '2357', '549', NULL, NULL, '-2328', '2869165', '9099', '2343', '0', '2729', '40919', '-2312', '16147', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (10, '136', '2719', '-97', '15500', '2412', '0', '-23', '531391', '630191', '100', '240', '302', '498', '2720', '19779', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (10, '12', '29074', '668', '2573', '3363314', NULL, '1943', '1421804', '570', '30154', '208', '630', '1941511', '510', '49219', '1');
INSERT INTO module_statuses (p_rout_id, module_id, param_0, param_1, param_10, param_11, param_12, param_2, param_3, param_4, param_5, param_6, param_7, param_8, param_9, status, version) VALUES (10, '11', '2309', '42', '109', '1592000', NULL, '25', '428200', '2340', '851', '-1952', '1078300', '2328', '80', '1555', '1');


--
-- Name: header_p_rout_id_index; Type: INDEX; Schema: logs; Owner: p-rout; Tablespace: 
--

CREATE INDEX header_p_rout_id_index ON header USING btree (p_rout_id);


--
-- Name: header_time_send_index; Type: INDEX; Schema: logs; Owner: p-rout; Tablespace: 
--

CREATE INDEX header_time_send_index ON header USING btree (time_send);


--
-- Name: module_statuses_p_rout_id_index; Type: INDEX; Schema: logs; Owner: p-rout; Tablespace: 
--

CREATE INDEX module_statuses_p_rout_id_index ON module_statuses USING btree (p_rout_id);


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

